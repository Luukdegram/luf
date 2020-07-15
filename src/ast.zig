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

    /// Frees memory of a node and all of its connected nodes.
    /// Altho this can be used to free the memory of all nodes,
    /// using an arena allocator will be much more performant.
    pub fn deinit(self: Node, allocator: *Allocator) void {
        switch (self) {
            .declaration => |decl| {
                decl.name.deinit(allocator);
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
            .boolean => |boolean| allocator.destroy(boolean),
            .block_statement => |blk| {
                for (blk.nodes) |n| {
                    n.deinit(allocator);
                }
                allocator.free(blk.nodes);
                allocator.destroy(blk);
            },
            .if_expression => |if_exp| {
                if_exp.condition.deinit(allocator);
                if (if_exp.true_pong) |pong| pong.deinit(allocator);
                if (if_exp.false_pong) |pong| pong.deinit(allocator);
                allocator.destroy(if_exp);
            },
            .func_lit => |fn_lit| {
                for (fn_lit.params) |param| {
                    param.deinit(allocator);
                }
                allocator.free(fn_lit.params);
                fn_lit.body.deinit(allocator);
                allocator.destroy(fn_lit);
            },
            .call_expression => |call_exp| {
                call_exp.function.deinit(allocator);
                for (call_exp.arguments) |arg| {
                    arg.deinit(allocator);
                }
                allocator.free(call_exp.arguments);
                allocator.destroy(call_exp);
            },
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
        name: Node,
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
        true_pong: ?Node = null,
        false_pong: ?Node = null,
    };

    /// Node representing a literal function which holds the function's
    /// parameters and the body of the function as a `BlockStatement`
    pub const FunctionLiteral = struct {
        token: Token,
        params: []Node,
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
