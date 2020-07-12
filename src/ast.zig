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
                .declaration => |decl| self.allocator.destroy(decl),
                .identifier => |id| self.allocator.destroy(id),
                ._return => |ret| self.allocator.destroy(ret),
                .prefix => |pref| self.allocator.destroy(pref),
                .infix => |inf| self.allocator.destroy(inf),
                .int_lit => |int| self.allocator.destroy(int),
                .expression => |exp| self.allocator.destroy(exp),
            }
        }
        self.allocator.free(self.nodes);
        self.allocator.free(self.tokens);
    }
};

/// Node represents a grammatical token within the language
/// i.e. a mutable statement such as mut x = 5
pub const Node = union(NodeType) {
    declaration: *Declaration,
    identifier: *Identifier,
    _return: *Return,
    prefix: *Prefix,
    infix: *Infix,
    int_lit: *IntegerLiteral,
    expression: *Expression,

    /// Possible Nodes which are supported
    pub const NodeType = enum {
        declaration,
        identifier,
        _return,
        prefix,
        infix,
        int_lit,
        expression,
    };

    /// Statement node -> const x = 5
    pub const Declaration = struct {
        token: Token,
        name: Identifier,
        value: Node,
    };

    /// Identifier node -> const x
    pub const Identifier = struct {
        token: Token,
        value: []const u8,
    };

    /// Return statement node -> return x
    pub const Return = struct {
        token: Token,
        value: Node,
    };

    /// Prefix node, used to determine order -> x + y * 5
    pub const Prefix = struct {
        token: Token,
        operator: Operator,
        right: Node,

        pub const Operator = enum {
            bool_not,
            minus,
            plus,
        };
    };

    /// Infix node, used to determine order
    pub const Infix = struct {
        token: Token,
        operator: enum {
            bool_or,
            bool_and,
            equal,
            not_equal,
            add,
            sub,
            mul,
            pow,
            div,

            assign,
            add_assign,
            sub_assign,
            mul_assign,
            pow_assign,
            div_assign,
        },
        right: Node,
        left: Node,
    };

    /// Represents an integer such as 385722
    pub const IntegerLiteral = struct {
        token: Token,
        value: usize,
    };

    /// Node to represent an expression, could be anything
    pub const Expression = struct {
        token: Token,
        expression: Node,
    };
};
