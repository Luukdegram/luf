const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const testing = std.testing;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const ast = @import("ast.zig");
const Node = ast.Node;
const Tree = ast.Tree;

/// Precendence represent the order of importance
/// The higher the value, the earlier it will be executed
/// This means a function call will be executed before a prefix,
/// and the product will be calculated before the sum.
const Precedence = enum {
    lowest,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,

    /// Returns the integer value of the enum
    fn val(self: Precedence) usize {
        return @enumToInt(self);
    }
};

/// Determines the Precendence based on the given Token Type
fn findPrecedence(token_type: Token.TokenType) Precedence {
    return switch (token_type) {
        .equal, .not_equal => .equals,
        .less_than, .greater_than => .less_greater,
        .plus, .minus => .sum,
        .slash, .asterisk => .product,
        .left_parenthesis => .call,
        else => .lowest,
    };
}

/// Parser retrieves tokens from our Lexer and turns them into
/// nodes to create an AST.
pub const Parser = struct {
    current_token: Token,
    peek_token: Token,
    allocator: *Allocator,
    tokens: []const Token,
    index: u32,
    source: []const u8,

    pub const Error = error{ParserError} || std.mem.Allocator.Error || std.fmt.ParseIntError;

    /// Creates a new Parser, using the given lexer.
    /// Sets the current and peek token.
    pub fn init(allocator: *Allocator, lexer: *Lexer) !Parser {
        const tokens = try lexer.tokenize(allocator);
        var parser = Parser{
            .current_token = undefined,
            .peek_token = undefined,
            .allocator = allocator,
            .tokens = tokens,
            .index = 2,
            .source = lexer.source,
        };

        // set current and peek token
        parser.current_token = parser.tokens[0];
        parser.peek_token = parser.tokens[1];
        return parser;
    }

    /// Sets the current token to the peek token and retrieves a new
    /// token from the Lexer and sets its value to the peak token.
    fn next(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.tokens[self.index];
        self.index += 1;
    }

    /// Parses the tokens that were generated by the lexer.
    /// Returns a `Tree` which owns the memory of the nodes.
    pub fn parse(self: *Parser) Error!Tree {
        var nodes = ArrayList(Node).init(self.allocator);
        defer nodes.deinit();

        while (!self.currentIsType(.eof)) {
            const node = try self.parseStatement();
            try nodes.append(node);

            if (!self.peekIsType(.eof)) {
                self.next();
            } else {
                break;
            }
        }

        return Tree{
            .nodes = try self.allocator.dupe(Node, nodes.items),
            .tokens = self.tokens,
            .allocator = self.allocator,
        };
    }

    /// Parses the statement into a node
    fn parseStatement(self: *Parser) Error!Node {
        return switch (self.current_token.type) {
            .constant, .mutable => self.parseDeclaration(),
            ._return => self.parseReturn(),
            else => self.parseExpressionStatement(),
        };
    }

    /// Parses a declaration
    fn parseDeclaration(self: *Parser) Error!Node {
        const tmp_token = self.current_token;

        if (!self.expectPeek(.identifier)) {
            return error.ParserError;
        }

        const val = self.source[self.current_token.start..self.current_token.end];
        const name = try self.allocator.create(Node.Identifier);
        name.* = .{ .token = self.current_token, .value = val };

        if (!self.expectPeek(.assign)) {
            return error.ParserError;
        }

        const decl = try self.allocator.create(Node.Declaration);
        decl.* = .{
            .token = tmp_token,
            .name = Node{ .identifier = name },
            .value = try self.parseExpression(.lowest),
        };

        return Node{ .declaration = decl };
    }

    /// Parses a return statement
    fn parseReturn(self: *Parser) Error!Node {
        const ret = try self.allocator.create(Node.Return);
        ret.* = .{ .token = self.current_token, .value = undefined };
        self.next();
        ret.value = try self.parseExpression(.lowest);
        return Node{ ._return = ret };
    }

    /// Parses the current token as an Identifier
    fn parseIdentifier(self: *Parser) Error!Node {
        const identifier = try self.allocator.create(Node.Identifier);
        const val = self.source[self.current_token.start..self.current_token.end];
        identifier.* = .{ .token = self.current_token, .value = val };
        return Node{ .identifier = identifier };
    }

    /// Parses an expression statement, determines which expression to parse based on the token
    fn parseExpressionStatement(self: *Parser) Error!Node {
        const statement = try self.allocator.create(Node.Expression);
        statement.* = .{ .token = self.current_token, .expression = try self.parseExpression(.lowest) };
        return Node{ .expression = statement };
    }

    /// Determines the correct expression type based on the current token type
    fn parseExpression(self: *Parser, prec: Precedence) Error!Node {
        var left = switch (self.current_token.type) {
            .identifier => try self.parseIdentifier(),
            .integer => try self.parseIntegerLiteral(),
            ._true, ._false => try self.parseBoolean(),
            ._if => try self.parseIfExpression(),
            .left_parenthesis => try self.parseGroupedExpression(),
            .function => try self.parseFunctionLiteral(),
            else => try self.parsePrefixExpression(),
        };

        if (prec.val() < findPrecedence(self.peek_token.type).val()) {
            self.next();
            left = try self.parseInfixExpression(left);
        }

        return left;
    }

    /// Parses the current token into a prefix, errors if current token is not a prefix token
    fn parsePrefixExpression(self: *Parser) Error!Node {
        const expression = try self.allocator.create(Node.Prefix);
        expression.* = .{ .token = self.current_token, .operator = self.current_token.string(), .right = undefined };

        self.next();

        expression.right = try self.parseExpression(.prefix);

        return Node{ .prefix = expression };
    }

    /// Parses the current token into an infix expression
    fn parseInfixExpression(self: *Parser, left: Node) Error!Node {
        const expression = try self.allocator.create(Node.Infix);
        expression.* = .{
            .token = self.current_token,
            .operator = self.current_token.string(),
            .left = left,
            .right = undefined,
        };

        const prec = findPrecedence(self.current_token.type);
        self.next();
        expression.right = try self.parseExpression(prec);

        return Node{ .infix = expression };
    }

    /// Parses the current token into an integer literal node
    fn parseIntegerLiteral(self: *Parser) Error!Node {
        const literal = try self.allocator.create(Node.IntegerLiteral);
        const string_number = self.source[self.current_token.start..self.current_token.end];
        const value = try std.fmt.parseInt(usize, string_number, 10);

        literal.* = .{ .token = self.current_token, .value = value };
        return Node{ .int_lit = literal };
    }

    /// Parses an expression into a boolean node
    fn parseBoolean(self: *Parser) !Node {
        const boolean = try self.allocator.create(Node.Bool);
        boolean.* = .{
            .token = self.current_token,
            .value = self.currentIsType(._true),
        };
        return Node{ .boolean = boolean };
    }

    /// Parses expressions into a grouped expression
    fn parseGroupedExpression(self: *Parser) Error!Node {
        self.next();
        const exp = try self.parseExpression(.lowest);
        if (!self.expectPeek(.right_parenthesis)) {
            return error.ParserError;
        }
        return exp;
    }

    /// Parses the expression into an if expression,
    /// captures the condition and block statements.
    fn parseIfExpression(self: *Parser) Error!Node {
        const exp = try self.allocator.create(Node.IfExpression);
        exp.* = .{
            .token = self.current_token,
            .condition = undefined,
            .true_pong = undefined,
            .false_pong = undefined,
        };
        if (!self.expectPeek(.left_parenthesis)) {
            return error.ParserError;
        }

        self.next();
        exp.condition = try self.parseExpression(.lowest);

        if (!self.expectPeek(.right_parenthesis)) {
            return error.ParserError;
        }

        if (!self.expectPeek(.left_brace)) {
            return error.ParserError;
        }

        exp.true_pong = try self.parseBlockStatement();

        if (self.peekIsType(._else)) {
            self.next();

            if (!self.expectPeek(.left_brace)) {
                return error.ParserError;
            }

            exp.false_pong = try self.parseBlockStatement();
        }

        return Node{ .if_expression = exp };
    }

    /// Parses the tokens into a `Node.BlockStatement` until a right brace is found
    fn parseBlockStatement(self: *Parser) !Node {
        const block = try self.allocator.create(Node.BlockStatement);
        errdefer self.allocator.destroy(block);
        block.* = .{
            .token = self.current_token,
            .nodes = undefined,
        };
        var list = ArrayList(Node).init(self.allocator);
        errdefer list.deinit();

        self.next(); //skip '{' token

        while (!self.currentIsType(.right_brace) and !self.currentIsType(.eof)) {
            const exp = try self.parseStatement();
            try list.append(exp);
            self.next();
        }

        block.nodes = list.toOwnedSlice();

        return Node{ .block_statement = block };
    }

    /// Parses current tokens into a function literal.
    /// Returns errors if no left parenthesis or brace is found.
    fn parseFunctionLiteral(self: *Parser) Error!Node {
        const func = try self.allocator.create(Node.FunctionLiteral);
        errdefer self.allocator.destroy(func);
        func.* = .{
            .token = self.current_token,
            .params = undefined,
            .body = undefined,
        };

        if (!self.expectPeek(.left_parenthesis)) {
            return error.ParserError;
        }

        func.params = try self.parseFunctionParameters();

        if (!self.expectPeek(.left_brace)) {
            return error.ParserError;
        }

        func.body = try self.parseBlockStatement();

        return Node{ .func_lit = func };
    }

    /// Parses the tokens into a list of nodes representing the parameters
    /// of a function literal.
    fn parseFunctionParameters(self: *Parser) Error![]Node {
        var list = ArrayList(Node).init(self.allocator);
        errdefer list.deinit();

        if (self.peekIsType(.right_parenthesis)) {
            self.next();
            return list.toOwnedSlice();
        }

        self.next();

        try list.append(try self.parseIdentifier());

        while (self.peekIsType(.comma)) {
            self.next();
            self.next();
            try list.append(try self.parseIdentifier());
        }

        if (!self.expectPeek(.right_parenthesis)) {
            return error.ParserError;
        }

        return list.toOwnedSlice();
    }

    /// Parses the current token into a call expression
    /// Accepts a function node
    fn parseCallExpression(self: *Parser, func: Node) Error!Node {
        std.debug.assert(func == .func_lit);
        const call = try self.allocator.create(Node.CallExpression);
        errdefer self.allocator.free(call);
        call.* = .{
            .token = self.current_token,
            .function = func,
            .arguments = try self.parseCallArguments(),
        };
        return Node{ .call_expression = call };
    }

    /// Parses the next set of tokens into argument nodes
    fn parseCallArguments(self: *Parser) Error![]Node {
        var list = ArrayList(Node).init(self.allocator);
        errdefer list.deinit();

        // no arguments
        if (self.peekIsType(.right_parenthesis)) {
            self.next();
            return list.toOwnedSlice();
        }

        self.next();
        try list.append(self.parseExpression(.lowest));

        while (self.peekIsType(.comma)) {
            self.next();
            self.next();
            try list.append(try self.parseExpression(.lowest));
        }

        if (!self.expectPeek(.right_parenthesis)) {
            return error.ParserError;
        }

        return list.toOwnedSlice();
    }

    /// Determines if the next token is the expected token or not.
    /// Incase the next token is the wanted token, retun true and retrieve next token.
    fn expectPeek(self: *Parser, token_type: Token.TokenType) bool {
        if (self.peekIsType(token_type)) {
            self.next();
            return true;
        }
        return false;
    }

    /// Helper function to check if the peek token is of given type
    fn peekIsType(self: Parser, token_type: Token.TokenType) bool {
        return @enumToInt(self.peek_token.type) == @enumToInt(token_type);
    }

    /// Helper function to check if the current token is of given type
    fn currentIsType(self: Parser, token_type: Token.TokenType) bool {
        return @enumToInt(self.current_token.type) == @enumToInt(token_type);
    }
};

test "Parse Delcaration" {
    const input =
        \\const x = 5
        \\mut y = 54
        \\const z = 1571
    ;

    var allocator = testing.allocator;
    var lexer = Lexer.init(input);
    var parser = try Parser.init(allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 3);

    const identifiers = &[_][]const u8{
        "x", "y", "z",
    };

    var index: usize = 0;
    for (tree.nodes) |node| {
        const id = identifiers[index];
        index += 1;

        testing.expectEqualSlices(u8, id, node.declaration.name.identifier.value);
    }
}

test "Parse Return statment" {
    const test_cases = .{
        .{ .input = "return 5", .expected = 5 },
        .{ .input = "return foo", .expected = "foo" },
        .{ .input = "return true", .expected = true },
    };

    var allocator = testing.allocator;
    inline for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();

        testing.expect(tree.nodes.len == 1);

        const node = tree.nodes[0]._return.value;

        switch (@typeInfo(@TypeOf(case.expected))) {
            .ComptimeInt => testing.expectEqual(@intCast(usize, case.expected), node.int_lit.value),
            .Pointer => testing.expectEqualSlices(u8, case.expected, node.identifier.value),
            .Bool => testing.expectEqual(case.expected, node.boolean.value),
            else => @panic("Unexpected type"),
        }
    }
}

test "Parse identifier expression" {
    const input = "foobar";

    var allocator = testing.allocator;
    var lexer = Lexer.init(input);
    var parser = try Parser.init(allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const identifier = tree.nodes[0].expression.expression.identifier;
    testing.expect(identifier.token.type == .identifier);
    testing.expectEqualSlices(u8, identifier.value, input);
}

test "Parse integer literal" {
    const input = "124";
    var allocator = testing.allocator;
    var lexer = Lexer.init(input);
    var parser = try Parser.init(allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const literal = tree.nodes[0].expression.expression.int_lit;
    testing.expect(literal.token.type == .integer);
    testing.expect(literal.value == 124);
}

test "Parse prefix expressions" {
    const VarValue = union {
        int: usize,
        string: []const u8,
        boolean: bool,
    };
    const TestCase = struct {
        input: []const u8,
        operator: []const u8,
        expected: VarValue,
    };
    const test_cases = &[_]TestCase{
        .{ .input = "-5", .operator = "-", .expected = VarValue{ .int = 5 } },
        .{ .input = "+25", .operator = "+", .expected = VarValue{ .int = 25 } },
        .{ .input = "!foobar", .operator = "!", .expected = VarValue{ .string = "foobar" } },
        .{ .input = "-foobar", .operator = "-", .expected = VarValue{ .string = "foobar" } },
        .{ .input = "!true", .operator = "!", .expected = VarValue{ .boolean = true } },
        .{ .input = "!false", .operator = "!", .expected = VarValue{ .boolean = false } },
    };

    const allocator = testing.allocator;
    for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();

        testing.expect(tree.nodes.len == 1);

        const prefix = tree.nodes[0].expression.expression.prefix;
        switch (prefix.right) {
            .int_lit => |int| testing.expect(case.expected.int == int.value),
            .identifier => |id| testing.expectEqualSlices(u8, case.expected.string, id.value),
            .boolean => |boolean| testing.expect(case.expected.boolean == boolean.value),
            else => @panic("Unexpected Node"),
        }

        testing.expectEqualSlices(u8, case.operator, prefix.operator);
    }
}

test "Parse infix expressions - integer" {
    const TestCase = struct {
        input: []const u8,
        left: usize,
        operator: []const u8,
        right: usize,
    };

    const test_cases = &[_]TestCase{
        .{ .input = "10 + 10", .left = 10, .operator = "+", .right = 10 },
        .{ .input = "10 - 10", .left = 10, .operator = "-", .right = 10 },
        .{ .input = "10 * 10", .left = 10, .operator = "*", .right = 10 },
        .{ .input = "10 / 10", .left = 10, .operator = "/", .right = 10 },
        .{ .input = "10 > 10", .left = 10, .operator = ">", .right = 10 },
        .{ .input = "10 < 10", .left = 10, .operator = "<", .right = 10 },
        .{ .input = "10 == 10", .left = 10, .operator = "==", .right = 10 },
        .{ .input = "10 != 10", .left = 10, .operator = "!=", .right = 10 },
    };

    const allocator = testing.allocator;
    for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();

        testing.expect(tree.nodes.len == 1);
        const node: Node = tree.nodes[0];
        const infix: *Node.Infix = node.expression.expression.infix;
        testing.expectEqualSlices(u8, case.operator, infix.operator);
        testing.expectEqual(case.left, infix.left.int_lit.value);
        testing.expectEqual(case.right, infix.right.int_lit.value);
    }
}

test "Parse infix expressions - identifier" {
    const allocator = testing.allocator;
    const string_test = "foobar + foobarz";
    var lexer = Lexer.init(string_test);
    var parser = try Parser.init(allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const node: Node = tree.nodes[0];
    const infix: *Node.Infix = node.expression.expression.infix;
    testing.expectEqualSlices(u8, "foobar", infix.left.identifier.value);
    testing.expectEqualSlices(u8, "foobarz", infix.right.identifier.value);
    testing.expectEqualSlices(u8, "+", infix.operator);
}

test "Parse infix expressions - boolean" {
    const allocator = testing.allocator;
    const boolean_test = "true == true";
    var lexer = Lexer.init(boolean_test);
    var parser = try Parser.init(allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const node: Node = tree.nodes[0];
    const infix: *Node.Infix = node.expression.expression.infix;
    testing.expectEqual(true, infix.left.boolean.value);
    testing.expectEqual(true, infix.right.boolean.value);
    testing.expectEqualSlices(u8, "==", infix.operator);
}

test "Boolean expression" {
    const allocator = testing.allocator;
    const boolean_test = "true";
    var lexer = Lexer.init(boolean_test);
    var parser = try Parser.init(allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    testing.expect(tree.nodes[0].expression.expression == .boolean);
}

test "If expression" {
    const allocator = testing.allocator;
    const boolean_test = "if (x < y) { x }";
    var lexer = Lexer.init(boolean_test);
    var parser = try Parser.init(allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const if_exp = tree.nodes[0].expression.expression.if_expression;
    testing.expect(if_exp.true_pong != null);
    testing.expect(if_exp.true_pong.?.block_statement.nodes[0] == .expression);
    testing.expectEqualSlices(
        u8,
        if_exp.true_pong.?.block_statement.nodes[0].expression.expression.identifier.value,
        "x",
    );
}

test "If else expression" {
    const allocator = testing.allocator;
    const boolean_test = "if (x < y) { x } else { y }";
    var lexer = Lexer.init(boolean_test);
    var parser = try Parser.init(allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const if_exp = tree.nodes[0].expression.expression.if_expression;
    testing.expect(if_exp.true_pong != null);
    testing.expect(if_exp.true_pong.?.block_statement.nodes[0] == .expression);
    testing.expectEqualSlices(
        u8,
        if_exp.true_pong.?.block_statement.nodes[0].expression.expression.identifier.value,
        "x",
    );
    testing.expect(if_exp.false_pong != null);
    testing.expect(if_exp.false_pong.?.block_statement.nodes[0] == .expression);
    testing.expectEqualSlices(
        u8,
        if_exp.false_pong.?.block_statement.nodes[0].expression.expression.identifier.value,
        "y",
    );
}

test "Function literal" {
    const input = "fn(x, y) { x + y }";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(testing.allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const func = tree.nodes[0].expression.expression.func_lit;
    testing.expect(func.params.len == 2);

    testing.expectEqualSlices(u8, func.params[0].identifier.value, "x");
    testing.expectEqualSlices(u8, func.params[1].identifier.value, "y");

    const body = func.body.block_statement.nodes[0];
    const infix = body.expression.expression.infix;
    testing.expectEqualSlices(u8, infix.operator, "+");
    testing.expectEqualSlices(u8, infix.left.identifier.value, "x");
    testing.expectEqualSlices(u8, infix.right.identifier.value, "y");
}

test "Function parameters" {
    const test_cases = .{
        .{ .input = "fn() {}", .expected = &[_][]const u8{} },
        .{ .input = "fn(x) {}", .expected = &[_][]const u8{"x"} },
        .{ .input = "fn(x, y, z) {}", .expected = &[_][]const u8{ "x", "y", "z" } },
    };

    inline for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(testing.allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();

        testing.expect(tree.nodes.len == 1);

        const func = tree.nodes[0].expression.expression.func_lit;
        testing.expect(func.params.len == case.expected.len);

        inline for (case.expected) |exp, i| {
            testing.expectEqualSlices(u8, exp, func.params[i].identifier.value);
        }
    }
}
