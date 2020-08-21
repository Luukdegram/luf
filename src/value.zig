const std = @import("std");
const ast = @import("ast.zig");
const Allocator = std.mem.Allocator;

//! Value represents a Zig to Luf type
//!

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
        func: BuiltinFn,
        arg_len: usize,
    },
    module: struct {
        name: []const u8,
        attributes: *Value,
    },
    iterable: struct {
        expose_index: bool,
        index: usize,
        value: *Value,

        /// Returns the next Value of the iterable.
        /// Returns null if empty iterable or the end of the iterator has been reached.
        /// NOTE: This returns the actual value, if you don't want the user to be able to modify
        /// this value, create a shallow copy of the return value.
        pub fn next(self: *@This()) ?*Value {
            switch (self.value.*) {
                .list => |list| {
                    if (list.items.len == 0) return null;
                    if (list.items.len == self.index) return null;

                    defer self.index += 1;
                    return list.items[self.index];
                },
                .string => |string| {
                    if (string.len == 0) return null;
                    if (string.len == self.index) return null;

                    defer self.index += 1;
                    //TODO
                    return null;
                },
                else => return null,
            }
        }
    },

    pub var True = Value{ .boolean = true };
    pub var False = Value{ .boolean = false };
    pub var Nil = Value{ .nil = {} };

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

    pub const List = std.ArrayList(*Value);
    pub const Map = std.HashMap(*const Value, *Value, hash, eql, true);

    pub const builtins = std.ComptimeStringMap(Value, .{
        .{ "len", len_func },
        .{ "add", add_func },
        .{ "pop", pop_func },
    });

    pub const builtin_keys = &[_][]const u8{
        "len", "add", "pop",
    };
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

pub const BuiltinError = error{ OutOfMemory, UnsupportedType, MismatchingTypes };
const BuiltinFn = fn (args: []*Value) BuiltinError!*Value;
const len_func = Value{ .native = .{ .func = len, .arg_len = 0 } };
const add_func = Value{ .native = .{ .func = add, .arg_len = 1 } };
const pop_func = Value{ .native = .{ .func = pop, .arg_len = 0 } };

/// Returns the length of the `Value`.
/// Supports strings, arrays and maps.
fn len(args: []*Value) BuiltinError!*Value {
    std.debug.assert(args.len == 1);
    const length: i64 = switch (args[0].*) {
        .string => |val| @intCast(i64, val.len),
        .list => |list| @intCast(i64, list.items.len),
        .map => |map| @intCast(i64, map.items().len),
        else => return BuiltinError.UnsupportedType,
    };
    return &Value{ .integer = length };
}

/// Appends a new value to the list
fn add(args: []*Value) BuiltinError!*Value {
    std.debug.assert(args.len >= 2);
    return switch (args[0].*) {
        .list => |*list| {
            const val = args[args.len - 1];
            if (list.items.len > 0) {
                if (list.items[0].* != std.meta.activeTag(val.*)) {
                    return BuiltinError.MismatchingTypes;
                }
            }
            try list.append(val);
            return args[0];
        },
        .map => |*map| {
            const key = args[args.len - 2];
            const val = args[args.len - 1];
            if (map.items().len > 0) {
                const entry = map.items()[0];
                if (entry.key.* != std.meta.activeTag(key.*)) {
                    return BuiltinError.MismatchingTypes;
                }
                if (entry.value.* != std.meta.activeTag(val.*)) {
                    return BuiltinError.MismatchingTypes;
                }
            }
            try map.put(key, val);
            return args[0];
        },
        else => BuiltinError.UnsupportedType,
    };
}

/// Pops the last argument of a list
fn pop(args: []*Value) BuiltinError!*Value {
    std.debug.assert(args.len == 1);
    return switch (args[0].*) {
        .list => |*list| {
            const val = list.pop();
            return val;
        },
        else => BuiltinError.UnsupportedType,
    };
}
