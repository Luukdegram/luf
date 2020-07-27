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
    string_lit: *StringLiteral,
    array: *ArrayLiteral,
    map: *MapLiteral,
    map_pair: *MapPair,
    index: *IndexExpression,

    /// Frees memory of a node and all of its connected nodes.
    /// Although this can be used to free memory recursively,
    /// using an arena allocator will be much more performant.
    pub fn deinit(self: Node, allocator: *Allocator) void {
        switch (self) {
            .declaration => |decl| {
                decl.name.deinit(allocator);
                decl.value.deinit(allocator);
                allocator.destroy(decl);
            },
            .identifier => |id| allocator.destroy(id),
            ._return => |ret| {
                ret.value.deinit(allocator);
                allocator.destroy(ret);
            },
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
                exp.value.deinit(allocator);
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
                if_exp.true_pong.deinit(allocator);
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
            .string_lit => |string| {
                allocator.free(string.value);
                allocator.destroy(string);
            },
            .array => |list| {
                for (list.value) |val| {
                    val.deinit(allocator);
                }
                allocator.free(list.value);
                allocator.destroy(list);
            },
            .index => |index| {
                index.left.deinit(allocator);
                index.index.deinit(allocator);
                allocator.destroy(index);
            },
            .map => |map| {
                for (map.value) |pair| {
                    pair.deinit(allocator);
                }
                allocator.free(map.value);
                allocator.destroy(map);
            },
            .map_pair => |pair| {
                pair.key.deinit(allocator);
                pair.value.deinit(allocator);
                allocator.destroy(pair);
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
        string_lit,
        array,
        map,
        map_pair,
        index,
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
            member,

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
                    .period => .member,
                    else => @panic("Unexpected token"),
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
};
