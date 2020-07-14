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
            node.deinit(self.allocator);
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
    block_statement: *BlockStatement,
    boolean: *Bool,
    if_expression: *IfExpression,
    func_lit: *FunctionLiteral,
    call_expression: *CallExpression,

    pub fn deinit(self: Node, allocator: *Allocator) void {
        switch (self) {
            .declaration => |decl| {
                decl.value.deinit(allocator);
                allocator.destroy(decl);
            },
            .identifier => |id| allocator.destroy(id),
            ._return => |ret| allocator.destroy(ret),
            .prefix => |pref| {
                pref.right.deinit(allocator);
                allocator.destroy(pref);
            },
            .infix => |inf| {
                inf.right.deinit(allocator);
                inf.left.deinit(allocator);
                allocator.destroy(inf);
            },
            .int_lit => |int| allocator.destroy(int),
            .expression => |exp| {
                exp.expression.deinit(allocator);
                allocator.destroy(exp);
            },
            .block_statement => |blk| {},
            .boolean => |boolean| {},
            .if_expression => |if_exp| {},
            .func_lit => |fn_lit| {},
            .call_expression => |call_exp| {},
        }
    }

    /// Possible Nodes which are supported
    pub const NodeType = enum {
        declaration,
        identifier,
        _return,
        prefix,
        infix,
        int_lit,
        expression,
        block_statement,
        boolean,
        if_expression,
        func_lit,
        call_expression,
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
        operator: []const u8,
        right: Node,
    };

    /// Infix node, used to determine order
    pub const Infix = struct {
        token: Token,
        operator: []const u8,
        right: Node,
        left: Node,
    };

    /// Represents an integer such as 385722
    pub const IntegerLiteral = struct {
        token: Token,
        value: usize,
    };

    /// Node to represent an expression, could be anything
    /// `.expression` contains the root node of the expression
    pub const Expression = struct {
        token: Token,
        expression: Node,
    };

    /// Node representing a block statement
    /// Nodes contains a slice of Nodes
    pub const BlockStatement = struct {
        token: Token,
        nodes: []Node,
    };

    /// Node representing a boolean
    pub const Bool = struct {
        token: Token,
        value: bool,
    };

    /// Represents an 'If' node
    pub const IfExpression = struct {
        token: Token,
        condition: Node,
        true_pong: Node,
        false_pong: Node,
    };

    /// Node representing a literal function which holds the function's
    /// parameters and the body of the function as a `BlockStatement`
    pub const FunctionLiteral = struct {
        token: Token,
        params: []*Identifier,
        body: Node,
    };

    /// Node representing a call expression and holds the function to be called
    /// and also the arguments to its function
    pub const CallExpression = struct {
        token: Token,
        function: Node,
        arguments: []Node,
    };
};
