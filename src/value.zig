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
    pub var True = _true.base;
    const _true = Boolean{ .base = .{ .l_type = .boolean }, .value = true };

    /// Global False value, saves memory by having it as a globally available 'constant'
    pub var False = _false.base;
    const _false = Boolean{ .base = .{ .l_type = .boolean }, .value = false };

    /// Global void value, saves memory by having it as a globally available 'constant'
    pub var Void = Value{ .l_type = ._void };

    /// Global Nil value, saves memory by having it as a globally available 'constant'
    pub var Nil = Value{ .l_type = .nil };

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

        /// Returns Luf's Type struct representing the enum value
        pub fn LufType(self: Type) type {
            return switch (self) {
                .integer => Integer,
                .boolean => Boolean,
                .string => String,
                .nil => void,
                ._return => ReturnValue,
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
                ReturnValue => ._return,
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
    pub fn unwrap(self: *Value, comptime l_type: Type) ?*l_type.LufType() {
        if (self.l_type != l_type) return null;

        return @fieldParentPtr(l_type.LufType(), "base", self);
    }

    /// Assures the type of `Value` is `String`
    pub fn toString(self: *Value) *String {
        return @fieldParentPtr(String, "base", self);
    }

    /// Assures the type of `Value` is `Integer`
    pub fn toInteger(self: *Value) *Integer {
        return @fieldParentPtr(Integer, "base", self);
    }

    /// Assures the type of `Value` is `Boolean`
    pub fn toBool(self: *Value) *Boolean {
        return @fieldParentPtr(Boolean, "base", self);
    }

    /// Assures the type of `Value` is `List`
    pub fn toList(self: *Value) *List {
        return @fieldParentPtr(List, "base", self);
    }

    /// Assures the type of `Value` is `Map`
    pub fn toMap(self: *Value) *Map {
        return @fieldParentPtr(Map, "base", self);
    }

    /// Assures the type of `Value` is `Function`
    pub fn toFunction(self: *Value) *Function {
        return @fieldParentPtr(Function, "base", self);
    }

    /// Assures the type of `Value` is `Native`
    pub fn toNative(self: *Value) *Native {
        return @fieldParentPtr(Native, "base", self);
    }

    /// Assures the type of `Value` is `Range`
    pub fn toRange(self: *Value) *Range {
        return @fieldParentPtr(Range, "base", self);
    }

    /// Assures the type of `Value` is `Enum`
    pub fn toEnum(self: *Value) *Enum {
        return @fieldParentPtr(Enum, "base", self);
    }

    /// Returns true if the `Type` of the given `Value` is equal
    pub fn isType(self: *const Value, tag: Type) bool {
        return self.l_type == tag;
    }

    /// Returns the `Value` as a Zig type
    /// TODO, make it an error when the Luf `Value` is not the corresponding type
    pub fn toZig(self: *const Value, comptime T: type) T {
        return switch (T) {
            void => {},
            bool => self.toBool().value,
            []const u8 => self.toString().value,
            *Map, *const Map => self.toMap().value,
            *Value, *const Value => self,
            Value => self.*,
            else => switch (@typeInfo(T)) {
                .Int => @intCast(T, self.toInteger().value),
                .Enum => @intToEnum(T, self.toInteger().value),
                else => @compileError("TODO add support for type: " ++ @typeName(T)),
            },
        };
    }

    /// Creates a Luf `Value` from a Zig value
    /// Memory is owned by caller and must be freed by caller
    pub fn fromZig(gc: *GarbageCollector, val: anytype) !*Value {
        switch (@TypeOf(val)) {
            // todo have a global void value
            void => return &Value{ .l_type = ._void, .next = null, .is_marked = false },
            *Value => return val,
            Value => {
                const ret = gc.gpa.create(Value);
                ret.* = val;
                return val;
            },
            bool => return Boolean.create(gc, val),
            []u8, []const u8 => return String.create(gc, val),
            type => switch (@typeInfo(val)) {
                .Struct => |info| {
                    const ret = try gc.newValue(Map, .map);
                    var map: Map = unwrap(.map).?;
                    map.value = std.AutoHashMapUnmanaged(*const Value, *Value){};
                    errdefer map.value.deinit(gc.gpa);

                    comptime var decl_count = 0;
                    inline for (info.decls) |decl| {
                        if (decl.is_pub) decl_count += 1;
                    }

                    try map.value.ensureCapacity(gc.gpa, decl_count);

                    inline for (indo.decls) |decl| {
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

                        fn call(_alloc: *Allocator, args: []*OldValue) !*OldValue {
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

                            return fromZig(_alloc, @call(.{}, innerFunc, tuple));
                        }
                    };
                    // set innerFunc as we cannot access `val` from inside
                    Func.innerFunc = val;

                    const ret = try gc.newValue(Native);
                    const native = ret.toNative();
                    native.func = Func.call;
                    native.arg_len = Fn.args.len;

                    return ret;
                },
                .ComptimeInt, .Int => return Integer.create(gc, val),
                .Pointer => |info| switch (@typeInfo(info.child)) {
                    .Array => |array_info| switch (array_info.child) {
                        u8 => return String.create(gc, val),
                        else => {
                            const ret = Value.List.create(gc, array_info.len);
                            for (list.value.items) |*item, i| {
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
        //std.debug.print("Destroying object: {}\n", .{self.*});
        switch (self.l_type) {
            .integer => self.toInteger().destroy(gpa),
            .boolean => self.toBool().destroy(gpa),
            .string => self.toString().destroy(gpa),
            .function => self.toFunction().destroy(gpa),
            .list => self.toList().destroy(gpa),
            .map => self.toMap().destroy(gpa),
            .module => self.unwrap(.module).?.destroy(gpa),
            .iterable => self.unwrap(.iterable).?.destroy(gpa),
            .range => self.toRange().destroy(gpa),
            ._enum => self.toEnum().destroy(gpa),
            else => unreachable,
        }
    }

    pub const Integer = struct {
        base: Value,
        value: i64,

        /// Creates a new `Integer` using the given `val`. Returns
        /// the pointer to its `Value`
        pub fn create(gc: *GarbageCollector, val: i64) !*Value {
            const value = try gc.newValue(Integer);
            const int = value.toInteger();
            int.value = val;

            return &int.base;
        }

        pub fn destroy(self: *Integer, gpa: *Allocator) void {
            gpa.destroy(self);
        }
    };

    pub const Boolean = struct {
        base: Value,
        value: bool,

        /// Creates a new `Boolean` using the given `val`. Returns
        /// the pointer to its `Value`
        pub fn create(gc: *GarbageCollector, val: bool) !*Value {
            const value = try gc.newValue(Boolean);
            const boolean = value.toBool();
            boolean.value = val;

            return &boolean.base;
        }

        pub fn destroy(self: *Boolean, gpa: *Allocator) void {
            gpa.destroy(self);
        }
    };

    pub const String = struct {
        base: Value,
        value: []const u8,

        /// Creates a new `Integer` using the given `val`. Returns
        /// the pointer to its `Value`
        pub fn create(gc: *GarbageCollector, val: []const u8) !*Value {
            const value = try gc.newValue(String);
            const string = value.toString();
            string.value = try gc.gpa.dupe(u8, val);
            return &string.base;
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
            const value = try gc.newValue(Module);
            const self = value.unwrap(.module).?;
            self.value = try gc.gpa.dupe(u8, val);

            return &self.base;
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
            const value = try gc.newValue(Function);
            const function = value.toFunction();
            function.arg_len = arg_len;
            function.locals = locals;
            function.name = if (name) |n| try gc.gpa.dupe(u8, n) else null;
            function.entry = entry_point;

            return &function.base;
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
            const ret = try gc.newValue(List);
            const self = ret.toList();
            self.value = ListType{};

            if (len) |n| try self.value.resize(gc.gpa, n);
            return ret;
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
            const ret = try gc.newValue(Map);
            const self = ret.toMap();
            self.value = MapType{};

            if (len) |n| try self.value.ensureCapacity(gc.gpa, n);
            return ret;
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
    };

    pub const Range = struct {
        base: Value,
        start: i64,
        end: i64,

        pub fn create(gc: *GarbageCollector, start: i64, end: i64) !*Value {
            const ret = try gc.newValue(Range);
            const range = ret.toRange();
            range.start = start;
            range.end = end;

            return ret;
        }

        pub fn destroy(self: *Range, gpa: *Allocator) void {
            gpa.destroy(self);
        }
    };

    pub const Enum = struct {
        base: Value,
        value: [][]const u8,

        pub fn create(gc: *GarbageCollector, enums: [][]const u8) !*Value {
            const ret = try gc.newValue(Enum);
            const enm = ret.toEnum();
            enm.value = enums;
            return ret;
        }

        pub fn destroy(self: *Enum, gpa: *Allocator) void {
            gpa.destroy(self);
        }
    };

    pub const Iterable = struct {
        base: Value,
        expose_index: bool,
        index: usize,
        value: *Value,

        pub fn create(gc: *GarbageCollector, expose: bool, index: usize, value: *Value) !*Value {
            const ret = try gc.newValue(Iterable);
            const it = ret.unwrap(.iterable).?;
            it.expose_index = expose;
            it.index = index;
            it.value = value;
            return ret;
        }

        pub fn destroy(self: *Iterable, gpa: *Allocator) void {
            gpa.destroy(self);
        }

        /// Returns a new Value, returns null if end of iterator is reached
        /// This creates a copy of the actual value
        pub fn next(self: *@This(), gc: *GarbageCollector) !?*Value {
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
                else => unreachable,
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

    pub const NativeFn = fn (gc: *GarbageCollector, args: []*Value) anyerror!*Value;
};

/// Value depending on its type
pub const OldValue = union(Value.Type) {
    integer: i64,
    boolean: bool,
    string: []const u8,
    _void,
    nil,
    module: []const u8,
    _return: *OldValue,
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
        value: *OldValue,

        /// Sets `value` to the next `Value` of the iterator.
        /// This creates a copy of the actual value
        pub fn next(self: *@This(), allocator: *Allocator, value: *OldValue) !void {
            value.* = OldValue.Nil;
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
    pub fn unwrapAs(self: *const OldValue, comptime tag: Value.Type) ?std.meta.TagPayloadType(OldValue, tag) {
        if (self.* != tag) return null;
        return @field(self, @tagName(tag));
    }

    /// Creates a new `Value` of type integer
    pub fn newInteger(value: i64) OldValue {
        return .{ .integer = value };
    }

    /// Creates a new `Value` of type function
    pub fn newFunction(name: ?[]const u8, locals: usize, arg_len: usize, entry_point: usize) OldValue {
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
    pub fn newString(value: []const u8) OldValue {
        return .{ .string = value };
    }

    /// Converts a `Value` to a Zig type of `T`
    pub fn toZig(self: *const OldValue, comptime T: type) T {
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
    pub fn fromZig(alloc: *std.mem.Allocator, val: anytype) !*OldValue {
        switch (@TypeOf(val)) {
            void => return &OldValue.Void,
            *Value => return val,
            Value => {
                const ret = alloc.create(OldValue);
                ret.* = val;
                return val;
            },
            bool => return if (val) &OldValue.True else &OldValue.False,
            []u8, []const u8 => {
                const ret = alloc.create(OldValue);
                ret.* = newString(val);
                return ret;
            },
            type => switch (@typeInfo(val)) {
                .Struct => |info| {
                    var map = OldValue.Map{};
                    errdefer map.deinit(alloc);

                    comptime var decl_count = 0;
                    inline for (info.decls) |decl| {
                        if (decl.is_pub) decl_count += 1;
                    }

                    try map.ensureCapacity(alloc, decl_count);

                    inline for (indo.decls) |decl| {
                        if (!decl.is_pub) continue;

                        const key = try alloc.create(OldValue);
                        key.* = newString(decl.name);

                        const value = try fromZig(allocator, @field(val, decl.name));

                        map.putAssumeCapacityNoClobber(key, value);
                    }

                    const ret = alloc.create(OldValue);
                    ret.* = .{ .map = map };
                    return ret;
                },
                else => @compileError("Unsupported type: " ++ @typeName(val)),
            },
            else => switch (@typeInfo(@TypeOf(val))) {
                .Fn => {
                    const ret = try alloc.create(OldValue);
                    const Fn = @typeInfo(@TypeOf(val)).Fn;

                    comptime var arg_index: u2 = 0;
                    const args_len = Fn.args.len;
                    const Func = struct {
                        // this is needed as `val` is not accessible from inner function
                        var innerFunc: @TypeOf(val) = undefined;

                        fn call(_alloc: *Allocator, args: []*OldValue) !*OldValue {
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

                            return OldValue.fromZig(_alloc, @call(.{}, innerFunc, tuple));
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
                    const ret = try alloc.create(OldValue);
                    ret.* = newInteger(val);
                    return ret;
                },
                .Pointer => |info| switch (@typeInfo(info.child)) {
                    .Array => |array_info| switch (array_info.child) {
                        u8 => {
                            const ret = try alloc.create(OldValue);
                            ret.* = OldValue.newString(val);
                            return ret;
                        },
                        else => {
                            const list = OldValue.List{};
                            errdefer list.deinit(alloc);

                            try list.ensureCapacity(alloc, array_info.len);
                            for (list.items) |*item, i| {
                                item.* = try fromZig(alloc, val[i]);
                            }

                            const ret = try alloc.create(OldValue);
                            ret.* = .{ .list = list };
                            return ret;
                        },
                    },
                    else => @compileError("Unsupported type: " ++ @typeName(@TypeOf(val))),
                },
                .Null => return &OldValue.Nil,
                else => @compileError("Unsupported type: " ++ @typeName(@TypeOf(val))),
            },
        }
    }

    pub var True = OldValue{ .boolean = true };
    pub var False = OldValue{ .boolean = false };
    pub var Nil: OldValue = .nil;
    pub var Void: OldValue = ._void;

    /// Frees Value's memory
    pub fn deinit(self: OldValue, alloc: *Allocator) void {
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

    fn hash(key: *const OldValue) u32 {
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

    fn eql(a: *const OldValue, b: *const OldValue) bool {
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
    pub fn isType(self: *const OldValue, tag: Value.Type) bool {
        return std.meta.activeTag(self.*) == tag;
    }

    /// Returns the `Type` of the `Value`
    pub fn lufType(self: *const OldValue) Value.Type {
        return std.meta.activeTag(self.*);
    }

    /// Returns the Zig type of `Value`
    pub fn zigType(self: *const OldValue) type {
        return std.meta.TagPayloadType(OldValue, self.lufType());
    }

    pub const List = std.ArrayListUnmanaged(*OldValue);
    pub const Map = std.ArrayHashMapUnmanaged(*const OldValue, *OldValue, hash, eql, true);
    pub const NativeFn = fn (allocator: *std.mem.Allocator, args: []*Value) anyerror!*Value;
    pub const LufFn = struct { name: ?[]const u8, arg_len: usize, locals: usize, entry: usize };

    /// Prints a `Value` to the given `writer`
    pub fn print(self: *const OldValue, writer: anytype) @TypeOf(writer).Error!void {
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
