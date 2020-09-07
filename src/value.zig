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
    module: []const u8,
    _return: *Value,
    function: LufFn,
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

    /// Converts a `Value` to a Zig type of `T`
    pub fn toZig(self: *const Value, comptime T: type) T {
        return switch (T) {
            void => {},
            bool => self.boolean,
            []const u8 => self.string,
            *Map, *const Map => self.map,
            *Value, *const Value => self,
            Value => self.*,
            else => switch (@typeInfo(T)) {
                .Int => @intCast(T, self.integer),
                .Enum => @intToEnum(T, self.integer),
                else => @compileError("TODO add support for type: " ++ @typeName(T)),
            },
        };
    }

    /// Creates a Luf `Value` from a Zig value
    /// Memory is owned by caller and must be freed by caller
    pub fn fromZig(alloc: *std.mem.Allocator, val: anytype) !*Value {
        switch (@TypeOf(val)) {
            void => return &Value.Void,
            *Value => return val,
            Value => {
                const ret = alloc.create(Value);
                ret.* = val;
                return val;
            },
            bool => return if (val) &Value.True else &Value.False,
            []u8, []const u8 => {
                const ret = alloc.create(Value);
                ret.* = newString(val);
                return ret;
            },
            type => switch (@typeInfo(val)) {
                .Struct => |info| {
                    var map = Value.Map{};
                    errdefer map.deinit(alloc);

                    comptime var decl_count = 0;
                    inline for (info.decls) |decl| {
                        if (decl.is_pub) decl_count += 1;
                    }

                    try map.ensureCapacity(alloc, decl_count);

                    inline for (indo.decls) |decl| {
                        if (!decl.is_pub) continue;

                        const key = try alloc.create(Value);
                        key.* = newString(decl.name);

                        const value = try fromZig(allocator, @field(val, decl.name));

                        map.putAssumeCapacityNoClobber(key, value);
                    }

                    const ret = alloc.create(Value);
                    ret.* = .{ .map = map };
                    return ret;
                },
                else => @compileError("Unsupported type: " ++ @typeName(val)),
            },
            else => switch (@typeInfo(@TypeOf(val))) {
                .Fn => {
                    const ret = try alloc.create(Value);
                    const Fn = @typeInfo(@TypeOf(val)).Fn;

                    comptime var arg_index: u2 = 0;
                    const args_len = Fn.args.len;
                    const Func = struct {
                        // this is needed as `val` is not accessible from inner function
                        var innerFunc: @TypeOf(val) = undefined;

                        fn call(_alloc: *Allocator, args: []*Value) !*Value {
                            if (args.len != args_len) return error.IncorrectArgumentCount;

                            const ArgsTuple = comptime blk: {
                                var fields: [args_len]std.builtin.TypeInfo.StructField = undefined;
                                for (fields) |*f, idx| {
                                    var num_buf: [128]u8 = undefined;
                                    f.* = .{
                                        .name = std.fmt.bufPrint(&num_buf, "{}", .{idx}) catch unreachable,
                                        .field_type = Fn.args[idx].arg_type.?,
                                        .default_value = @as(?(Fn.args[idx].arg_type.?), null),
                                        .is_comptime = false,
                                    };
                                }
                                break :blk @Type(.{
                                    .Struct = .{
                                        .layout = .Auto,
                                        .fields = &fields,
                                        .decls = &[0]std.builtin.TypeInfo.Declaration{},
                                        .is_tuple = true,
                                    },
                                });
                            };

                            var tuple: ArgsTuple = undefined;
                            comptime var i = 0;
                            inline while (i < args_len) : (i += 1) {
                                tuple[i] = args[i].toZig(Fn.args[i].arg_type.?);
                            }

                            return Value.fromZig(_alloc, @call(.{}, innerFunc, tuple));
                        }
                    };
                    // set innerFunc as we cannot access `val` from inside
                    Func.innerFunc = val;

                    ret.* = .{
                        .native = .{
                            .func = Func.call,
                            .arg_len = Fn.args.len,
                        },
                    };

                    return ret;
                },
                .ComptimeInt, .Int => {
                    const ret = try alloc.create(Value);
                    ret.* = newInteger(val);
                    return ret;
                },
                .Pointer => |info| switch (@typeInfo(info.child)) {
                    .Array => |array_info| switch (array_info.child) {
                        u8 => {
                            const ret = try alloc.create(Value);
                            ret.* = Value.newString(val);
                            return ret;
                        },
                        else => {
                            const list = Value.List{};
                            errdefer list.deinit(alloc);

                            try list.ensureCapacity(alloc, array_info.len);
                            for (list.items) |*item, i| {
                                item.* = try fromZig(alloc, val[i]);
                            }

                            const ret = try alloc.create(Value);
                            ret.* = .{ .list = list };
                            return ret;
                        },
                    },
                    else => @compileError("Unsupported type: " ++ @typeName(@TypeOf(val))),
                },
                .Null => return &Value.Nil,
                else => @compileError("Unsupported type: " ++ @typeName(@TypeOf(val))),
            },
        }
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
    pub const NativeFn = fn (allocator: *std.mem.Allocator, args: []*Value) anyerror!*Value;
    pub const LufFn = struct { name: ?[]const u8, arg_len: usize, locals: usize, entry: usize };

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
