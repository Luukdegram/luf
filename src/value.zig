const std = @import("std");
const ast = @import("ast.zig");
const Allocator = std.mem.Allocator;
const GarbageCollector = @import("gc.zig").GarbageCollector;

/// Luf Value
pub const Value = struct {
    /// actual type of the `Value`
    l_type: Type,
    /// is marked by the gc?
    is_marked: bool = false,
    /// Next Value in the Linked List
    next: ?*Value = null,

    /// Global True value, saves memory by having it as a globally available 'constant'
    pub var True = Boolean{ .base = .{ .l_type = .boolean }, .value = true };

    /// Global False value, saves memory by having it as a globally available 'constant'
    pub var False = Boolean{ .base = .{ .l_type = .boolean }, .value = false };

    /// Global void value, saves memory by having it as a globally available 'constant'
    pub var Void = Value{ .l_type = ._void };

    /// Global Nil value, saves memory by having it as a globally available 'constant'
    pub var Nil = Value{ .l_type = .nil };

    /// Signature for native functions, has access to garbage collector so values created
    /// by a native function can be cleaned up as well
    pub const NativeFn = fn (gc: *GarbageCollector, args: []*Value) anyerror!*Value;

    /// Build in types supported by Luf
    pub const Type = enum {
        integer,
        boolean,
        string,
        nil,
        function,
        list,
        map,
        native,
        module,
        iterable,
        range,
        _enum,
        _void,

        /// Returns Luf's Type struct representing the enum value
        pub fn LufType(self: Type) type {
            return switch (self) {
                .integer => Integer,
                .boolean => Boolean,
                .string => String,
                .nil => @TypeOf(null),
                .function => Function,
                .list => List,
                .map => Map,
                .native => Native,
                .module => Module,
                .iterable => Iterable,
                .range => Range,
                ._enum => Enum,
                ._void => void,
            };
        }

        /// Returns `Type` based on given struct
        /// Compiler error when the type is not one within the scope of `Value`
        pub fn fromType(comptime T: type) Type {
            return switch (T) {
                Integer => .integer,
                Boolean => .boolean,
                String => .string,
                Function => .function,
                List => .list,
                Map => .map,
                Native => .native,
                Module => .module,
                Iterable => .iterable,
                Range => .range,
                Enum => ._enum,
                else => unreachable,
            };
        }
    };

    /// Unwraps the `Value` into the actual Type
    /// Returns null if Value is of different type
    pub fn unwrap(self: *Value, comptime l_type: Type) ?*l_type.LufType() {
        if (self.l_type != l_type) return null;

        return @fieldParentPtr(l_type.LufType(), "base", self);
    }

    /// Casts `Value` to `String`
    /// User must ensure Value contains correct type
    pub fn toString(self: *Value) *String {
        return @fieldParentPtr(String, "base", self);
    }

    /// Casts `Value` to `Integer`
    /// User must ensure Value contains correct type
    pub fn toInteger(self: *Value) *Integer {
        return @fieldParentPtr(Integer, "base", self);
    }

    /// Casts `Value` to `Boolean`
    /// User must ensure Value contains correct type
    pub fn toBool(self: *Value) *Boolean {
        return @fieldParentPtr(Boolean, "base", self);
    }

    /// Casts `Value` to `List`
    /// User must ensure Value contains correct type
    pub fn toList(self: *Value) *List {
        return @fieldParentPtr(List, "base", self);
    }

    /// Casts `Value` to `Map`
    /// User must ensure Value contains correct type
    pub fn toMap(self: *Value) *Map {
        return @fieldParentPtr(Map, "base", self);
    }

    /// Casts `Value` to `Native`
    /// User must ensure Value contains correct type
    pub fn toFunction(self: *Value) *Function {
        return @fieldParentPtr(Function, "base", self);
    }

    /// Casts `Value` to `Native`
    /// User must ensure Value contains correct type
    pub fn toNative(self: *Value) *Native {
        return @fieldParentPtr(Native, "base", self);
    }

    /// Casts `Value` to `Range`
    /// User must ensure Value contains correct type
    pub fn toRange(self: *Value) *Range {
        return @fieldParentPtr(Range, "base", self);
    }

    /// Casts `Value` to `Enum`
    /// User must ensure Value contains correct type
    pub fn toEnum(self: *Value) *Enum {
        return @fieldParentPtr(Enum, "base", self);
    }

    /// Casts `Value` to `Iterable`
    /// User must ensure Value contains correct type
    pub fn toIterable(self: *Value) *Iterable {
        return @fieldParentPtr(Iterable, "base", self);
    }

    /// Casts `Value` to `Module`
    /// User must ensure Value contains correct type
    pub fn toModule(self: *Value) *Module {
        return @fieldParentPtr(Module, "base", self);
    }

    /// Returns true if the `Type` of the given `Value` is equal
    pub fn isType(self: *const Value, tag: Type) bool {
        return self.l_type == tag;
    }

    /// Returns the `Value` as a Zig type
    /// `error.InvalidType` is returned when `Value` cannot be coerced to the given
    /// Zig type
    pub fn toZig(self: *Value, comptime T: type) error{InvalidType}!T {
        return switch (T) {
            void => {},
            bool => if (self.unwrap(.boolean)) |b| b.value else error.InvalidType,
            []const u8 => if (self.unwrap(.string)) |s| s.value else error.InvalidType,
            *Map, *const Map => if (self.unwrap(.map)) |map| map.value else error.InvalidType,
            *Value, *const Value => self,
            Value => self.*,
            else => switch (@typeInfo(T)) {
                .Null => if (self.unwrap(.nil)) |nil| null else error.InvalidType,
                .Int => @intCast(T, if (self.unwrap(.integer)) |int| int.value else return error.InvalidType),
                .Enum => @intToEnum(T, if (self.unwrap(.integer)) |int| int.value else return error.InvalidType),
                else => @compileError("TODO add support for type: " ++ @typeName(T)),
            },
        };
    }

    /// Creates a Luf `Value` from a Zig value
    /// Memory is owned by caller and must be freed by caller
    pub fn fromZig(gc: *GarbageCollector, val: anytype) !*Value {
        switch (@TypeOf(val)) {
            // todo have a global void value
            void => return &Void,
            *Value => return val,
            Value => {
                const ret = gc.gpa.create(Value);
                ret.* = val;
                return val;
            },
            bool => return if (val) &Value.True.base else &Value.False.base,
            []u8, []const u8 => return String.create(gc, val),
            type => switch (@typeInfo(val)) {
                .Struct => |info| {
                    comptime var decl_count = 0;
                    inline for (info.decls) |decl| {
                        if (decl.is_pub) decl_count += 1;
                    }

                    const ret = try Map.create(gc, decl_count);
                    const map = ret.toMap();
                    errdefer ret.destroy(gc.gpa);

                    try map.value.ensureCapacity(gc.gpa, decl_count);

                    inline for (info.decls) |decl| {
                        if (!decl.is_pub) continue;

                        const key = try String.create(gc, decl.name);
                        const value = try fromZig(gc, @field(val, decl.name));

                        map.value.putAssumeCapacityNoClobber(key, value);
                    }

                    return ret;
                },
                else => @compileError("Unsupported type: " ++ @typeName(val)),
            },
            else => switch (@typeInfo(@TypeOf(val))) {
                .Fn => {
                    const Fn = @typeInfo(@TypeOf(val)).Fn;

                    comptime var arg_index: u2 = 0;
                    const args_len = Fn.args.len;
                    const Func = struct {
                        // this is needed as `val` is not accessible from inner function
                        var innerFunc: @TypeOf(val) = undefined;

                        fn call(_gc: *GarbageCollector, args: []*Value) !*Value {
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
                                tuple[i] = try args[i].toZig(Fn.args[i].arg_type.?);
                            }

                            return fromZig(_gc, @call(.{}, innerFunc, tuple));
                        }
                    };
                    // set innerFunc as we cannot access `val` from inside
                    Func.innerFunc = val;

                    return try gc.newValue(
                        Native,
                        .{
                            .base = .{ .l_type = .native, .next = null, .is_marked = false },
                            .func = Func.call,
                            .arg_len = Fn.args.len,
                        },
                    );
                },
                .ComptimeInt, .Int => return Integer.create(gc, @intCast(i64, val)),
                .Pointer => |info| switch (@typeInfo(info.child)) {
                    .Array => |array_info| switch (array_info.child) {
                        u8 => return String.create(gc, val),
                        else => {
                            const ret = try Value.List.create(gc, array_info.len);
                            for (ret.toList().value.items) |*item, i| {
                                item.* = try fromZig(gc, val[i]);
                            }

                            return ret;
                        },
                    },
                    else => @compileError("Unsupported type: " ++ @typeName(@TypeOf(val))),
                },
                .Null => return &Nil,
                else => @compileError("Unsupported type: " ++ @typeName(@TypeOf(val))),
            },
        }
    }

    /// Frees all memory of the `Value`.
    /// NOTE, for lists/maps it only frees the list/map itself, not the values it contains
    pub fn destroy(self: *Value, gpa: *Allocator) void {
        switch (self.l_type) {
            .integer => self.toInteger().destroy(gpa),
            .string => self.toString().destroy(gpa),
            .function => self.toFunction().destroy(gpa),
            .list => self.toList().destroy(gpa),
            .map => self.toMap().destroy(gpa),
            .module => self.toModule().destroy(gpa),
            .iterable => self.toIterable().destroy(gpa),
            .range => self.toRange().destroy(gpa),
            ._enum => self.toEnum().destroy(gpa),
            .native => self.toNative().destroy(gpa),
            else => {},
        }
    }

    pub const Integer = struct {
        base: Value,
        value: i64,

        /// Creates a new `Integer` using the given `val`. Returns
        /// the pointer to its `Value`
        pub fn create(gc: *GarbageCollector, val: i64) !*Value {
            return try gc.newValue(
                Integer,
                .{
                    .base = .{ .l_type = .integer, .next = null, .is_marked = false },
                    .value = val,
                },
            );
        }

        pub fn destroy(self: *Integer, gpa: *Allocator) void {
            gpa.destroy(self);
        }
    };

    pub const Boolean = struct {
        base: Value,
        value: bool,
    };

    pub const String = struct {
        base: Value,
        value: []const u8,

        /// Creates a new `Integer` using the given `val`. Returns
        /// the pointer to its `Value`
        pub fn create(gc: *GarbageCollector, val: []const u8) !*Value {
            return try gc.newValue(
                String,
                .{
                    .base = .{ .l_type = .string, .next = null, .is_marked = false },
                    .value = try gc.gpa.dupe(u8, val),
                },
            );
        }

        /// Copies the value of `other` into `self`
        /// Does this by first free'ing the current value, and then duplicating
        /// the value of `other`. This does NOT free the value of the original `other`
        pub fn copyFrom(self: *String, gpa: *Allocator, other: *String) !void {
            gpa.free(self.value);
            self.value = try gpa.dupe(u8, other.value);
        }

        pub fn destroy(self: *String, gpa: *Allocator) void {
            gpa.free(self.value);
            gpa.destroy(self);
        }
    };

    pub const Module = struct {
        base: Value,
        value: []const u8,

        pub fn create(gc: *GarbageCollector, val: []const u8) !*Value {
            return try gc.newValue(
                Module,
                .{
                    .base = .{ .l_type = .module, .next = null, .is_marked = false },
                    .value = try gc.gpa.dupe(u8, val),
                },
            );
        }

        pub fn destroy(self: *Module, gpa: *Allocator) void {
            gpa.free(self.value);
            gpa.destroy(self);
        }
    };

    pub const ReturnValue = struct {
        base: Value,
        value: *Value,
    };

    pub const Function = struct {
        base: Value,
        name: ?[]const u8,
        arg_len: usize,
        locals: usize,
        entry: usize,

        /// Creates a new `Integer` using the given `val`. Returns
        /// the pointer to its `Value`
        pub fn create(
            gc: *GarbageCollector,
            name: ?[]const u8,
            locals: usize,
            arg_len: usize,
            entry_point: usize,
        ) !*Value {
            const n = if (name) |n| try gc.gpa.dupe(u8, n) else null;
            return try gc.newValue(
                Function,
                .{
                    .base = .{ .l_type = .function, .next = null, .is_marked = false },
                    .arg_len = arg_len,
                    .locals = locals,
                    .name = n,
                    .entry = entry_point,
                },
            );
        }

        pub fn destroy(self: *Function, gpa: *Allocator) void {
            if (self.name) |name| gpa.free(name);
            gpa.destroy(self);
        }
    };

    pub const List = struct {
        base: Value,
        value: ListType,

        pub const ListType = std.ArrayListUnmanaged(*Value);

        pub fn create(gc: *GarbageCollector, len: ?usize) !*Value {
            var self = List{
                .base = .{ .l_type = .list, .next = null, .is_marked = false },
                .value = ListType{},
            };

            if (len) |n| try self.value.ensureCapacity(gc.gpa, n);
            return try gc.newValue(List, self);
        }

        pub fn destroy(self: *List, gpa: *Allocator) void {
            self.value.deinit(gpa);
            gpa.destroy(self);
        }
    };

    pub const Map = struct {
        base: Value,
        value: MapType,

        pub const MapType = std.ArrayHashMapUnmanaged(*Value, *Value, hash, eql, true);

        pub fn create(gc: *GarbageCollector, len: ?usize) !*Value {
            var self = Map{
                .base = .{ .l_type = .map, .next = null, .is_marked = false },
                .value = MapType{},
            };

            if (len) |n| try self.value.ensureCapacity(gc.gpa, n);
            return try gc.newValue(Map, self);
        }

        pub fn destroy(self: *Map, gpa: *Allocator) void {
            self.value.deinit(gpa);
            gpa.destroy(self);
        }
    };

    pub const Native = struct {
        base: Value,
        func: Value.NativeFn,
        arg_len: usize,

        pub fn destroy(self: *Native, gpa: *Allocator) void {
            gpa.destroy(self);
        }
    };

    pub const Range = struct {
        base: Value,
        start: i64,
        end: i64,

        pub fn create(gc: *GarbageCollector, start: i64, end: i64) !*Value {
            return try gc.newValue(
                Range,
                .{
                    .base = .{ .l_type = .range, .next = null, .is_marked = false },
                    .start = start,
                    .end = end,
                },
            );
        }

        pub fn destroy(self: *Range, gpa: *Allocator) void {
            gpa.destroy(self);
        }
    };

    pub const Enum = struct {
        base: Value,
        value: [][]const u8,

        pub fn create(gc: *GarbageCollector, enums: [][]const u8) !*Value {
            return try gc.newValue(
                Enum,
                .{
                    .base = .{ .l_type = ._enum, .next = null, .is_marked = false },
                    .value = enums,
                },
            );
        }

        pub fn destroy(self: *Enum, gpa: *Allocator) void {
            gpa.free(self.value);
            gpa.destroy(self);
        }
    };

    pub const Iterable = struct {
        base: Value,
        expose_index: bool,
        index: usize,
        value: *Value,

        pub fn create(gc: *GarbageCollector, expose: bool, index: usize, value: *Value) !*Value {
            return try gc.newValue(
                Iterable,
                .{
                    .base = .{ .l_type = .iterable, .next = null, .is_marked = false },
                    .expose_index = expose,
                    .index = index,
                    .value = value,
                },
            );
        }

        pub fn destroy(self: *Iterable, gpa: *Allocator) void {
            gpa.destroy(self);
        }

        /// Returns a new Value, returns null if end of iterator is reached
        /// This creates a copy of the actual value
        pub fn next(self: *@This(), gc: *GarbageCollector) !?*Value {
            std.debug.print("it val: {}\n", .{self.value.l_type});
            switch (self.value.l_type) {
                .list => {
                    const list = self.value.toList().value;
                    if (list.items.len == 0) return null;
                    if (list.items.len == self.index) return null;

                    defer self.index += 1;
                    return list.items[self.index];
                },
                .string => {
                    const string = self.value.toString().value;
                    if (string.len == 0) return null;
                    if (string.len == self.index) return null;

                    defer self.index += 1;
                    return String.create(gc, string[self.index .. self.index + 1]);
                },
                .range => {
                    const range = self.value.toRange();
                    if (range.start == range.end) return null;
                    if (self.index == range.end - range.start) return null;

                    defer self.index += 1;
                    return Integer.create(gc, range.start + @intCast(i64, self.index));
                },
                else => return null,
            }
        }
    };

    fn hash(key: *Value) u32 {
        const hashFn = std.hash.autoHash;
        var hasher = std.hash.Wyhash.init(0);

        switch (key.l_type) {
            .integer => hashFn(&hasher, key.toInteger().value),
            .boolean => hashFn(&hasher, key.toBool().value),
            .string => hasher.update(key.toString().value),
            .function => {
                const func = key.toFunction();
                if (func.name) |name|
                    hasher.update(name);
                hashFn(&hasher, func.arg_len);
                hashFn(&hasher, func.locals);
                hashFn(&hasher, func.entry);
            },
            .list => {
                const list = key.toList().value;
                hashFn(&hasher, list.items.len);
                hashFn(&hasher, list.items.ptr);
            },
            .map => {
                const map = key.toMap().value;
                hashFn(&hasher, map.items().len);
                hashFn(&hasher, map.items().ptr);
            },
            .native => {
                const native = key.toNative();
                hashFn(&hasher, native.arg_len);
                hashFn(&hasher, native.func);
            },
            .range => {
                const range = key.toRange();
                hashFn(&hasher, range.start);
                hashFn(&hasher, range.end);
            },
            .nil => {},
            else => unreachable,
        }
        return @truncate(u32, hasher.final());
    }

    fn eql(a: *Value, b: *Value) bool {
        return switch (a.l_type) {
            .integer => a.toInteger().value == b.toInteger().value,
            .boolean => a.toBool().value == b.toBool().value,
            .nil => true,
            .string => std.mem.eql(u8, a.toString().value, b.toString().value),
            .list => {
                const list_a = a.toList().value;
                const list_b = b.toList().value;

                if (list_a.items.len != list_b.items.len) return false;
                for (list_a.items) |item, i| {
                    if (!item.eql(list_b.items[i])) return false;
                }
                return true;
            },
            .map => {
                const map_a = a.toMap().value;
                const map_b = a.toMap().value;

                if (map_a.items().len != map_b.items().len) return false;
                for (map_a.items()) |entry, i| {
                    if (entry.hash != map_b.items()[i].hash) return false;
                }
                return true;
            },
            .range => {
                const range_a = a.toRange();
                const range_b = b.toRange();

                return range_a.start == range_b.start and range_a.end == range_b.end;
            },
            else => unreachable,
        };
    }

    /// Prints a `Value` to the given `writer`
    pub fn print(self: *Value, writer: anytype) @TypeOf(writer).Error!void {
        switch (self.l_type) {
            .integer => try writer.print("{}", .{self.toInteger().value}),
            .boolean => try writer.print("{}", .{self.toBool().value}),
            .string => try writer.writeAll(self.toString().value),
            .nil => try writer.writeAll("nil"),
            .list => {
                const list = self.toList().value;
                try writer.writeAll("[");
                for (list.items) |item, i| {
                    try item.print(writer);
                    if (i != list.items.len - 1)
                        try writer.writeAll(",\n");
                }
                try writer.writeAll("]\n");
            },
            .map => {
                const map = self.toMap().value;
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
            .range => try writer.print("{}..{}", .{ self.toRange().start, self.toRange().end }),
            ._enum => {
                const enm = self.toEnum().value;
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
