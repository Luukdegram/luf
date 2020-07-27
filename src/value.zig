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
            else => alloc.destroy(self),
        }
    }

    pub const List = std.ArrayList(Value);

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

    pub const Map = std.StringHashMap(Value);

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

const builtinFn = fn (args: []Value) error{ OutOfMemory, UnsupportedType, MismatchingTypes }!*Value;
const len_func = Value{ .builtin = .{ .func = len } };
const add_func = Value{ .builtin = .{ .func = add } };

/// Returns the length of the `Value`.
/// Supports strings, arrays and maps.
fn len(args: []Value) !*Value {
    std.debug.assert(args.len == 1);
    const length: i64 = switch (args[0]) {
        .string => |val| @intCast(i64, val.len),
        .list => |list| @intCast(i64, list.items.len),
        else => return error.UnsupportedType,
    };
    return &Value{ .integer = length };
}

/// Appends a new value to the list
fn add(args: []Value) !*Value {
    std.debug.assert(args.len == 2);
    return switch (args[0]) {
        .list => |*list| {
            if (list.items.len > 0) {
                if (list.items[0] != std.meta.activeTag(args[1])) {
                    return error.MismatchingTypes;
                }
            }
            try list.append(args[1]);
            return &args[0];
        },
        else => error.UnsupportedType,
    };
}
