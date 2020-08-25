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
    nil,
    _return: *Value,
    function: struct {
        arg_len: usize,
        locals: usize,
        instructions: []const @import("bytecode.zig").Instruction,
    },
    list: List,
    map: Map,
    native: struct {
        func: NativeFn,
        arg_len: usize,
    },
    module: struct {
        name: []const u8,
        attributes: *Value,
    },
    range: struct {
        start: i64,
        end: i64,
    },
    _enum: [][]const u8,
    _void,
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

    pub var True = Value{ .boolean = true };
    pub var False = Value{ .boolean = false };
    pub var Nil: Value = .nil;
    pub var Void: Value = ._void;

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
                hashFn(&hasher, func.arg_len);
                hashFn(&hasher, func.locals);
                hashFn(&hasher, func.instructions.len);
                hashFn(&hasher, func.instructions.ptr);
            },
            .list => |list| {
                hashFn(&hasher, list.items.len);
                hashFn(&hasher, list.items.ptr);
            },
            .map => |map| {
                hashFn(&hasher, map.items().len);
                hashFn(&hasher, map.items().ptr);
            },
            .native => |native| hashFn(&hasher, native.func),
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
    pub const Map = std.HashMapUnmanaged(*const Value, *Value, hash, eql, true);
    pub const NativeFn = fn (vm: *@import("vm.zig").Vm, args: []*Value) anyerror!*Value;
};

/// Scope maps identifiers to their names and can be used
/// to store and retrieve the value of an identifier.
pub const Scope = struct {
    store: Map,
    parent: ?*Scope = null,
    allocator: *Allocator,

    const Map = std.StringHashMap(Value);

    pub fn init(allocator: *Allocator) Scope {
        return Scope{ .store = Map.init(allocator), .allocator = allocator };
    }

    /// Creates a new `Value` on the stack of the scope
    pub fn create(self: Scope) !*Value {
        return self.allocator.create(Value);
    }

    /// Creates a new Scope from the given scope where the parent is set
    /// to the given Scope.
    pub fn clone(self: *Scope) !*Scope {
        const scope = try self.allocator.create(Scope);
        scope.* = .{
            .store = Map.init(self.allocator),
            .parent = self,
            .allocator = self.allocator,
        };
        return scope;
    }

    /// Returns null if identifier does not exist within the Scope or any its parent scopes,
    /// else returns its value
    pub fn get(self: Scope, name: []const u8) ?Value {
        var val = self.store.get(name);
        if (val == null and self.parent != null) {
            val = self.parent.?.get(name);
        }

        return val;
    }

    /// Saves the identifier and its value. Will overwrite any existing identifier
    /// TODO: Constness checking
    pub fn set(self: *Scope, name: []const u8, value: Value) !void {
        return self.store.put(name, value);
    }

    /// Frees Scope's memory.
    /// Note: This only frees the memory of the Scope and its internal map,
    /// not the objects stored inside.
    pub fn deinit(self: *Scope) void {
        self.store.deinit();
        self.* = undefined;
    }
};
