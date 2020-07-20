const std = @import("std");
const ast = @import("ast.zig");

/// Build in types supported by Luf
pub const Type = enum {
    integer,
    boolean,
    nil,
    _return,
    function,
};

/// Value depending on its type
pub const Value = union(Type) {
    integer: i64,
    boolean: bool,
    nil: void,
    _return: *Value,
    function: struct {
        params: []ast.Node,
        body: ast.Node,
        scope: Scope,
    },
};

/// Scope maps identifiers to their names and can be used
/// to store and retrieve the value of an identifier.
pub const Scope = struct {
    store: Map,
    parent: ?*Scope = null,

    pub const Map = std.StringHashMap(Value);

    pub fn init(allocator: *std.mem.Allocator) Scope {
        return Scope{ .store = Map.init(allocator) };
    }

    /// Creates a new Scope from the given scope where the parent is set
    /// to the given Scope.
    pub fn clone(self: *Scope, allocator: *std.mem.Allocator) Scope {
        return Scope{ .store = Map.init(allocator), .parent = self };
    }

    /// Returns null if identifier does not exist, else returns its value
    pub fn get(self: Scope, name: []const u8) ?Value {
        return self.store.get(name);
    }

    /// Saves the identifier and its value. Will overwrite any existing identifier
    /// TODO: Type checking
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
