const std = @import("std");

/// Build in types supported by Luf
pub const Type = enum {
    integer,
    boolean,
    nil,
    _return,
};

/// Value depending on its type
pub const Value = union(Type) {
    integer: i64,
    boolean: bool,
    nil: void,
    _return: *Value,
};

/// Env maps identifiers to their names and can be used
/// to store and retrieve the value of an identifier.
pub const Env = struct {
    store: Map,

    pub const Map = std.StringHashMap(Value);

    pub fn init(allocator: *std.mem.Allocator) Env {
        return Env{ .store = Map.init(allocator) };
    }

    /// Returns null if identifier does not exist, else returns its value
    pub fn get(self: Env, name: []const u8) ?Value {
        return self.store.get(name);
    }

    /// Saves the identifier and its value. Will overwrite any existing identifier
    /// TODO: Type checking
    /// TODO: Constness checking
    pub fn set(self: *Env, name: []const u8, value: Value) !void {
        return self.store.put(name, value);
    }

    pub fn deinit(self: *Env) void {
        //self.store.clearAndFree();
        self.store.deinit();
    }
};
