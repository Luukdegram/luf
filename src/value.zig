const std = @import("std");
const ast = @import("ast.zig");
const Allocator = std.mem.Allocator;

//! Supported Values by Luf

/// Build in types supported by Luf
pub const Type = enum {
    integer,
    boolean,
    string,
    nil,
    _return,
    function,
    list,
    map,
    native,
    module,
    iterable,
    range,
    _enum,
    _void,
};

/// Value depending on its type
pub const Value = union(Type) {
    integer: i64,
    boolean: bool,
    string: []const u8,
    _void,
    nil,
    module,
    _return: *Value,
    function: struct {
        name: ?[]const u8,
        arg_len: usize,
        locals: usize,
        entry: usize,
    },
    list: List,
    map: Map,
    native: struct {
        func: NativeFn,
        arg_len: usize,
    },
    range: struct {
        start: i64,
        end: i64,
    },
    _enum: [][]const u8,
    iterable: struct {
        expose_index: bool,
        index: usize,
        value: *Value,

        /// Sets `value` to the next `Value` of the iterator.
        /// This creates a copy of the actual value
        pub fn next(self: *@This(), allocator: *Allocator, value: *Value) !void {
            value.* = Value.Nil;
            switch (self.value.*) {
                .list => |list| {
                    if (list.items.len == 0) return;
                    if (list.items.len == self.index) return;

                    defer self.index += 1;
                    value.* = list.items[self.index].*;
                },
                .string => |string| {
                    if (string.len == 0) return;
                    if (string.len == self.index) return;

                    defer self.index += 1;
                    value.* = .{ .string = try allocator.dupe(u8, string[self.index .. self.index + 1]) };
                },
                .range => |range| {
                    if (range.start == range.end) return;
                    if (self.index == range.end - range.start) return;

                    defer self.index += 1;
                    value.* = .{ .integer = range.start + @intCast(i64, self.index) };
                },
                else => {},
            }
        }
    },

    /// Unwraps the `Value` union into the type of the active tag.
    /// Returns null if the wanted tag is not active.
    pub fn unwrapAs(self: *const Value, comptime tag: Type) ?std.meta.TagPayloadType(Value, tag) {
        if (self.* != tag) return null;
        return @field(self, @tagName(tag));
    }

    /// Creates a new `Value` of type integer
    pub fn newInteger(value: i64) Value {
        return .{ .integer = value };
    }

    /// Creates a new `Value` of type function
    pub fn newFunction(name: ?[]const u8, locals: usize, arg_len: usize, entry_point: usize) Value {
        return .{
            .function = .{
                .name = name,
                .arg_len = arg_len,
                .locals = locals,
                .entry = entry_point,
            },
        };
    }

    /// Creates a new `Value` of type string
    pub fn newString(value: []const u8) Value {
        return .{ .string = value };
    }

    pub var True = Value{ .boolean = true };
    pub var False = Value{ .boolean = false };
    pub var Nil: Value = .nil;
    pub var Void: Value = ._void;
    pub var Module: Value = .module;

    /// Frees Value's memory
    pub fn deinit(self: Value, alloc: *Allocator) void {
        switch (self) {
            .string => |val| {
                alloc.free(val);
                alloc.destroy(self);
            },
            .function => |func| {
                func.scope.deinit();
                alloc.destroy(func);
            },
            .list => |list| list.deinit(),
            .map => |map| map.deinit(),
            .function => |func| {
                alloc.free(func.instructions);
                alloc.destroy(func);
            },
            .module => |mod| {
                alloc.free(mod.name);
                alloc.destroy(mod);
            },
            else => alloc.destroy(self),
        }
    }

    fn hash(key: *const Value) u32 {
        const hashFn = std.hash.autoHash;
        var hasher = std.hash.Wyhash.init(0);

        switch (key.*) {
            .integer => |int| hashFn(&hasher, int),
            .boolean => |boolean| hashFn(&hasher, boolean),
            .string => |str| hasher.update(str),
            .function => |func| {
                if (func.name) |name|
                    hasher.update(name);
                hashFn(&hasher, func.arg_len);
                hashFn(&hasher, func.locals);
                hashFn(&hasher, func.entry);
            },
            .list => |list| {
                hashFn(&hasher, list.items.len);
                hashFn(&hasher, list.items.ptr);
            },
            .map => |map| {
                hashFn(&hasher, map.items().len);
                hashFn(&hasher, map.items().ptr);
            },
            .native => |native| {
                hashFn(&hasher, native.arg_len);
                hashFn(&hasher, native.func);
            },
            .range => |range| {
                hashFn(&hasher, range.start);
                hashFn(&hasher, range.end);
            },
            .nil => {},
            else => unreachable,
        }
        return @truncate(u32, hasher.final());
    }

    fn eql(a: *const Value, b: *const Value) bool {
        return switch (a.*) {
            .integer => a.integer == b.integer,
            .boolean => a.boolean == b.boolean,
            .nil => true,
            .string => std.mem.eql(u8, a.string, b.string),
            .list => |list| {
                if (list.items.len != b.list.items.len) return false;
                for (list.items) |item, i| {
                    if (!item.eql(b.list.items[i])) return false;
                }
                return true;
            },
            .map => |map| {
                if (map.items().len != b.map.items().len) return false;
                for (map.items()) |entry, i| {
                    if (entry.hash != b.map.items()[i].hash) return false;
                }
                return true;
            },
            .range => |range| return range.start == b.range.start and range.end == b.range.end,
            else => unreachable,
        };
    }

    /// Returns true if the `Value` is of given type `Type`
    pub fn isType(self: *const Value, tag: Type) bool {
        return std.meta.activeTag(self.*) == tag;
    }

    /// Returns the `Type` of the `Value`
    pub fn lufType(self: *const Value) Type {
        return std.meta.activeTag(self.*);
    }

    /// Returns the Zig type of `Value`
    pub fn zigType(self: *const Value) type {
        return std.meta.TagPayloadType(Value, self.lufType());
    }

    pub const List = std.ArrayListUnmanaged(*Value);
    pub const Map = std.ArrayHashMapUnmanaged(*const Value, *Value, hash, eql, true);
    pub const NativeFn = fn (vm: *@import("vm.zig").Vm, args: []*Value) anyerror!*Value;

    /// Prints a `Value` to the given `writer`
    pub fn print(self: *const Value, writer: anytype) @TypeOf(writer).Error!void {
        switch (self.*) {
            .integer => |int| try writer.print("{}", .{int}),
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .string => |string| try writer.writeAll(string),
            .nil => |nil| try writer.writeAll("nil"),
            .list => |list| {
                try writer.writeAll("[");
                for (list.items) |item, i| {
                    try item.print(writer);
                    if (i != list.items.len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("]\n");
            },
            .map => |map| {
                try writer.writeAll("{");
                for (map.entries.items) |item, i| {
                    try item.key.print(writer);
                    try writer.writeAll(":");
                    try item.value.print(writer);
                    if (i != map.items().len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("}\n");
            },
            .range => |range| try writer.print("{}..{}", .{ range.start, range.end }),
            ._enum => |enm| {
                try writer.writeAll("{");
                for (enm) |item, i| {
                    try writer.writeAll(item);
                    if (i != enm.len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("}\n");
            },
            else => try writer.writeAll("void"),
        }
    }
};
