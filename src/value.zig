const std = @import("std");
const ast = @import("ast.zig");
const Allocator = std.mem.Allocator;

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
    builtin,
};

/// Value depending on its type
pub const Value = union(Type) {
    integer: i64,
    boolean: bool,
    string: []const u8,
    nil: void,
    _return: *Value,
    function: struct {
        params: []ast.Node,
        body: ast.Node,
        scope: *Scope,
    },
    list: List,
    map: Map,
    builtin: struct {
        func: builtinFn,
    },

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
            else => alloc.destroy(self),
        }
    }

    fn hash(key: Value) u32 {
        const hashFn = std.hash.autoHash;
        var hasher = std.hash.Wyhash.init(0);

        switch (key) {
            .integer => |int| hashFn(&hasher, int),
            .boolean => |boolean| hashFn(&hasher, boolean),
            .string => |str| hasher.update(str),
            .function => |func| {
                hashFn(&hasher, func.params.len);
                hashFn(&hasher, func);
            },
            .list => |list| {
                hashFn(&hasher, list.items.len);
                hashFn(&hasher, list.items.ptr);
            },
            .map => |map| {
                hashFn(&hasher, map.items().len);
                hashFn(&hasher, map.items().ptr);
            },
            .builtin => |builtin| hashFn(&hasher, builtin.func),
            .nil => {},
            else => unreachable,
        }
        return @truncate(u32, hasher.final());
    }

    fn eql(a: Value, b: Value) bool {
        return switch (a) {
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

    pub const List = std.ArrayList(Value);
    pub const Map = std.HashMap(Value, Value, hash, eql, true);

    pub const builtins = std.ComptimeStringMap(Value, .{
        .{ "len", len_func },
        .{ "add", add_func },
    });
};

/// Scope maps identifiers to their names and can be used
/// to store and retrieve the value of an identifier.
pub const Scope = struct {
    store: Map,
    parent: ?*Scope = null,
    allocator: *std.mem.Allocator,

    const Map = std.StringHashMap(Value);

    pub fn init(allocator: *std.mem.Allocator) Scope {
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

const BuiltinError = error{ OutOfMemory, UnsupportedType, MismatchingTypes };
const builtinFn = fn (args: []Value) BuiltinError!*Value;
const len_func = Value{ .builtin = .{ .func = len } };
const add_func = Value{ .builtin = .{ .func = add } };

/// Returns the length of the `Value`.
/// Supports strings, arrays and maps.
fn len(args: []Value) BuiltinError!*Value {
    std.debug.assert(args.len == 1);
    const length: i64 = switch (args[0]) {
        .string => |val| @intCast(i64, val.len),
        .list => |list| @intCast(i64, list.items.len),
        .map => |map| @intCast(i64, map.items().len),
        else => return BuiltinError.UnsupportedType,
    };
    return &Value{ .integer = length };
}

/// Appends a new value to the list
fn add(args: []Value) BuiltinError!*Value {
    std.debug.assert(args.len >= 2);
    return switch (args[0]) {
        .list => |*list| {
            if (list.items.len > 0) {
                if (list.items[0] != std.meta.activeTag(args[1])) {
                    return BuiltinError.MismatchingTypes;
                }
            }
            try list.append(args[1]);
            return &args[0];
        },
        .map => |*map| {
            if (map.items().len > 0) {
                const entry = map.items()[0];
                if (entry.key != std.meta.activeTag(args[1])) {
                    return BuiltinError.MismatchingTypes;
                }
                if (entry.value != std.meta.activeTag(args[2])) {
                    return BuiltinError.MismatchingTypes;
                }
            }
            try map.put(args[1], args[2]);
            return &args[0];
        },
        else => BuiltinError.UnsupportedType,
    };
}
