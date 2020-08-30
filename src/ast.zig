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
    @"return": *Return,
    prefix: *Prefix,
    infix: *Infix,
    int_lit: *IntegerLiteral,
    expression: *Expression,
    block_statement: *BlockStatement,
    boolean: *Bool,
    if_expression: *IfExpression,
    func_lit: *FunctionLiteral,
    func_arg: *FunctionArgument,
    call_expression: *CallExpression,
    string_lit: *StringLiteral,
    array: *ArrayLiteral,
    map: *MapLiteral,
    map_pair: *MapPair,
    index: *IndexExpression,
    while_loop: *WhileLoop,
    for_loop: *ForLoop,
    assignment: *Assignment,
    comment: *Comment,
    nil: *Nil,
    import: *Import,
    @"continue": *Continue,
    @"break": *Break,
    range: *Range,
    @"enum": *EnumLiteral,
    switch_statement: *SwitchLiteral,
    switch_prong: *SwitchProng,
    type_def: *TypeDef,

    /// Possible Nodes which are supported
    pub const NodeType = enum {
        declaration,
        identifier,
        @"return",
        prefix,
        infix,
        int_lit,
        expression,
        block_statement,
        boolean,
        if_expression,
        func_lit,
        func_arg,
        call_expression,
        string_lit,
        array,
        map,
        map_pair,
        index,
        while_loop,
        for_loop,
        assignment,
        comment,
        nil,
        import,
        @"continue",
        @"break",
        range,
        @"enum",
        switch_statement,
        switch_prong,
        type_def,
    };

    /// Returns the Luf `Type` that Node corresponds to
    pub fn getType(self: Node) @import("value.zig").Type {
        return switch (self) {
            .declaration => |x| if (x.type_def) |td| td.getType() else x.value.getType(),
            .@"return" => |ret| ret.value.getType(),
            .prefix => |pre| pre.right.getType(),
            .int_lit => .integer,
            .expression => |exp| exp.value.getType(),
            .boolean => .boolean,
            .func_lit => .function,
            .func_arg => |arg| arg.arg_type.getType(),
            .string_lit => .string,
            .array => .list,
            .map => .map,
            .nil => .nil,
            .import => .module,
            .range => .range,
            .@"enum" => ._enum,
            .type_def => |td| td.getType(),
            else => unreachable,
        };
    }

    /// Represents a String
    pub const StringLiteral = struct {
        token: Token,
        value: []const u8,
    };

    /// Node representing an array
    pub const ArrayLiteral = struct {
        token: Token,
        value: []const Node,
    };

    /// Node representing a map
    pub const MapLiteral = struct {
        token: Token,
        value: []const Node,
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
        type_def: ?Node,
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
            bitwise_not,

            pub fn fromToken(token: Token) Op {
                return switch (token.token_type) {
                    .minus => .minus,
                    .bang => .bang,
                    .tilde => .bitwise_not,
                    else => unreachable,
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
            mod,
            multiply,
            divide,
            less_than,
            greater_than,
            equal,
            not_equal,
            bitwise_xor,
            bitwise_or,
            bitwise_and,
            not,
            shift_left,
            shift_right,
            @"and",
            @"or",
            assign_add,
            assign_sub,
            assign_mul,
            assign_div,

            /// Returns the corresponding `Op` based on the given `Token`
            pub fn fromToken(token: Token) Op {
                return switch (token.token_type) {
                    .plus => .add,
                    .minus => .sub,
                    .assign => .assign,
                    .asterisk => .multiply,
                    .percent => .mod,
                    .slash => .divide,
                    .less_than => .less_than,
                    .greater_than => .greater_than,
                    .equal => .equal,
                    .not_equal => .not_equal,
                    .ampersand => .bitwise_and,
                    .caret => .bitwise_xor,
                    .tilde => .not,
                    .pipe => .bitwise_or,
                    .shift_left => .shift_left,
                    .shift_right => .shift_right,
                    .@"and" => .@"and",
                    .@"or" => .@"or",
                    .equal_add => .assign_add,
                    .equal_div => .assign_div,
                    .equal_mul => .assign_mul,
                    .equal_sub => .assign_sub,
                    else => unreachable,
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
        nodes: []const Node,
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
        params: []const Node,
        body: ?Node,
        ret_type: Node,
    };

    /// Represents an argument inside a function, contains both the
    /// identifier as the type of the argument.
    pub const FunctionArgument = struct {
        token: Token,
        value: []const u8,
        arg_type: Node,
    };

    /// Node representing a call expression and holds the function to be called
    /// and also the arguments to its function
    pub const CallExpression = struct {
        token: Token,
        function: Node,
        arguments: []const Node,
    };

    /// Represents a while loop, contains the condition and the block
    pub const WhileLoop = struct {
        token: Token,
        condition: Node,
        block: Node,
    };

    pub const ForLoop = struct {
        token: Token,
        iter: Node,
        capture: Node,
        index: ?Node,
        block: Node,
    };

    /// Represents an assignment '=' for setting the value of an existing variable
    pub const Assignment = struct {
        token: Token,
        left: Node,
        right: Node,
    };

    /// Represents a single line comment
    pub const Comment = struct {
        token: Token,
        value: []const u8,
    };

    /// Represents "Nil"
    pub const Nil = struct { token: Token };

    /// Represents the Node which imports other Luf files
    pub const Import = struct {
        token: Token,
        value: Node,
    };

    /// "break" statement
    pub const Break = struct { token: Token };
    /// "continue" statement
    pub const Continue = struct { token: Token };

    /// Represents a range i.e. 0..128
    pub const Range = struct {
        token: Token,
        left: Node,
        right: Node,
    };

    /// Represents an Enum declaration i.e. const my_enum = enum{}
    pub const EnumLiteral = struct {
        token: Token,
        nodes: []const Node,
    };

    /// Represents a Switch statement i.e. switch(x) {}
    pub const SwitchLiteral = struct {
        token: Token,
        capture: Node,
        prongs: []const Node,
    };

    /// Represents the pong inside a switch statement, i.e.
    pub const SwitchProng = struct {
        token: Token,
        left: Node,
        right: Node,
    };

    /// Represents the type of the parent node
    pub const TypeDef = struct {
        token: Token,
        value: ?Node,

        /// Returns a Luf `Type` based on the token of the `TypeDef`
        pub fn getType(self: *const TypeDef) @import("value.zig").Type {
            return switch (self.token.token_type) {
                .bool_type => .boolean,
                .string_type => .string,
                .int_type => .integer,
                .void_type => ._void,
                .function => self.value.?.getType(),
                .left_bracket => self.value.?.getType(),
                else => unreachable,
            };
        }
    };
};
