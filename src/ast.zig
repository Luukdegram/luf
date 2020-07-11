const Allocator = @import("std").mem.Allocator;
const Token = @import("token.zig").Token;

/// Tree represents all parsed Nodes
pub const Tree = struct {
    nodes: []Node,
    tokens: []const Token,
    allocator: *Allocator,

    /// Frees all memory
    pub fn deinit(self: Tree) void {
        for (self.nodes) |node| {
            switch (node) {
                .statement => |stmt| self.allocator.destroy(stmt),
                .identifier => |id| self.allocator.destroy(id),
            }
        }
        self.allocator.free(self.nodes);
        self.allocator.free(self.tokens);
    }
};

/// Possible Nodes which are supported
pub const NodeType = enum {
    statement,
    identifier,
};

/// Node represents a grammatical token within the language
/// i.e. a mutable statement such as mut x = 5
pub const Node = union(NodeType) {
    statement: *Statement,
    identifier: *Identifier,

    /// Statement node -> const x = 5
    pub const Statement = struct {
        token: Token,
        name: Identifier,
        value: ?*Node,
    };

    /// Identifier node -> const x
    pub const Identifier = struct {
        token: Token,
        value: []const u8,
    };
};
