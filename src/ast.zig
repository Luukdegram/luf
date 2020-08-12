const std = @import("std");
const Token = @import("token.zig").Token;
const Errors = @import("error.zig").Errors;

//! All AST Nodes are defined here
//! The `Parser` parses all of the `Lexer`'s tokens into these nodes

/// Tree represents all parsed Nodes
pub const Tree = struct {
    nodes: []const Node,
    arena: std.heap.ArenaAllocator.State,
    allocator: *std.mem.Allocator,
    errors: Errors,

    /// Frees all memory
    pub fn deinit(self: *Tree) void {
        self.arena.promote(self.allocator).deinit();
        self.allocator.destroy(self);
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
    string_lit: *StringLiteral,
    array: *ArrayLiteral,
    map: *MapLiteral,
    map_pair: *MapPair,
    index: *IndexExpression,
    while_loop: *WhileLoop,
    assignment: *Assignment,
    comment: *Comment,

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
        string_lit,
        array,
        map,
        map_pair,
        index,
        while_loop,
        assignment,
        comment,
    };

    /// Represents a String
    pub const StringLiteral = struct {
        token: Token,
        value: []const u8,
    };

    /// Node representing an array
    pub const ArrayLiteral = struct {
        token: Token,
        value: []Node,
    };

    /// Node representing a map
    pub const MapLiteral = struct {
        token: Token,
        value: []Node,
    };

    /// Node presents a key/value pair for inside a `MapLiteral`
    pub const MapPair = struct {
        token: Token,
        key: Node,
        value: Node,
    };

    /// Represents an index selector to retrieve a value from an Array or Map
    pub const IndexExpression = struct {
        token: Token,
        left: Node,
        index: Node,
    };

    /// Statement node -> const x = 5
    pub const Declaration = struct {
        token: Token,
        name: Node,
        value: Node,
        mutable: bool = false,
    };

    /// Identifier node -> x
    pub const Identifier = struct {
        token: Token,
        value: []const u8,
    };

    /// Return statement node -> return x
    pub const Return = struct {
        token: Token,
        value: Node,
    };

    /// Prefix node, determines order such as negation
    pub const Prefix = struct {
        token: Token,
        operator: Op,
        right: Node,

        pub const Op = enum {
            minus,
            bang,

            pub fn fromToken(token: Token) Op {
                return switch (token.type) {
                    .minus => .minus,
                    .bang => .bang,
                    else => @panic("Unexpected token"),
                };
            }
        };
    };

    /// Infix node, used to determine order for arithmetics
    pub const Infix = struct {
        token: Token,
        operator: Op,
        right: Node,
        left: Node,

        pub const Op = enum {
            assign,
            add,
            sub,
            multiply,
            divide,
            less_than,
            greater_than,
            equal,
            not_equal,

            /// Returns the corresponding `Op` based on the given `Token`
            pub fn fromToken(token: Token) Op {
                return switch (token.type) {
                    .plus => .add,
                    .minus => .sub,
                    .assign => .assign,
                    .asterisk => .multiply,
                    .slash => .divide,
                    .less_than => .less_than,
                    .greater_than => .greater_than,
                    .equal => .equal,
                    .not_equal => .not_equal,
                    else => @panic("Unexpected token"), //TODO replace this hard panic
                };
            }
        };
    };

    /// Represents an integer such as 385722
    pub const IntegerLiteral = struct {
        token: Token,
        value: u64,
    };

    /// Node to represent an expression, could be anything
    /// `value` contains the root node of the expression
    pub const Expression = struct {
        token: Token,
        value: Node,
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

    /// Represents a while loop, contains the condition and the block
    pub const WhileLoop = struct {
        token: Token,
        condition: Node,
        block: Node,
    };

    /// Represents an assignment '=' for setting the value of an existing variable
    pub const Assignment = struct {
        token: Token,
        name: Node,
        value: Node,
    };

    /// Represents a single line comment
    pub const Comment = struct {
        token: Token,
        value: []const u8,
    };
};
