const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const testing = std.testing;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const ast = @import("ast.zig");
const Node = ast.Node;
const Tree = ast.Tree;

/// Parser retrieves tokens from our Lexer and turns them into
/// nodes to create an AST.
pub const Parser = struct {
    current_token: Token,
    peek_token: Token,
    allocator: *Allocator,
    tokens: []const Token,
    index: u32,

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

        while (true) {
            if (self.parseStatement()) |node| {
                try nodes.append(node);

                if (!self.peekIsType(.eof)) {
                    self.next();
                } else {
                    break;
                }
            } else |err| {
                std.debug.print("Err: {}\n", .{err});
                return err;
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

        const name = Node.Identifier{ .token = self.current_token, .value = self.current_token.literal };
        if (!self.expectPeek(.assign)) {
            return error.ParserError;
        }

        self.next();

        const decl = try self.allocator.create(Node.Declaration);
        decl.* = .{
            .token = tmp_token,
            .name = name,
            .value = try self.parseExpression(),
        };

        return Node{ .declaration = decl };
    }

    /// Parses a return statement
    fn parseReturn(self: *Parser) Error!Node {
        const ret = try self.allocator.create(Node.Return);
        ret.* = .{ .token = self.current_token, .value = undefined };
        return Node{ ._return = ret };
    }

    /// Parses the current token as an Identifier
    fn parseIdentifier(self: *Parser) Error!Node {
        const identifier = try self.allocator.create(Node.Identifier);
        identifier.* = .{ .token = self.current_token, .value = self.current_token.literal };
        return Node{ .identifier = identifier };
    }

    /// Parses an expression statement, determines which expression to parse based on the token
    fn parseExpressionStatement(self: *Parser) Error!Node {
        const statement = try self.allocator.create(Node.Expression);
        statement.* = .{ .token = self.current_token, .expression = try self.parseExpression() };
        return Node{ .expression = statement };
    }

    /// Determines the correct expression type based on the current token type
    fn parseExpression(self: *Parser) Error!Node {
        return switch (self.current_token.type) {
            .identifier => self.parseIdentifier(),
            .integer => self.parseIntegerLiteral(),
            else => self.parsePrefixExpression(),
        };
    }

    /// Parses the current token into a prefix, errors if current token is not a prefix token
    fn parsePrefixExpression(self: *Parser) Error!Node {
        const prefix: Node.Prefix.Operator = switch (self.current_token.type) {
            .minus => .minus,
            .plus => .plus,
            .not_equal => .bool_not,
            else => return error.ParserError,
        };

        const temp = self.current_token;

        self.next();

        const expression = try self.allocator.create(Node.Prefix);
        expression.* = .{ .token = temp, .operator = prefix, .right = try self.parseExpression() };

        return Node{ .prefix = expression };
    }

    /// Parses the current token into an integer literal node
    fn parseIntegerLiteral(self: *Parser) Error!Node {
        const literal = try self.allocator.create(Node.IntegerLiteral);
        const value = try std.fmt.parseInt(usize, self.current_token.literal, 10);

        literal.* = .{ .token = self.current_token, .value = value };
        return Node{ .int_lit = literal };
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
        if (node == .declaration) {
            const id = identifiers[index];
            index += 1;

            testing.expectEqualSlices(u8, id, node.declaration.name.value);
            testing.expectEqualSlices(u8, node.declaration.name.value, node.declaration.name.token.literal);
        }
    }
}

test "Parse Return statment" {
    const input =
        \\return 5
        \\return 10
        \\return 13957
    ;

    var allocator = testing.allocator;
    var lexer = Lexer.init(input);
    var parser = try Parser.init(allocator, &lexer);
    const tree = try parser.parse();
    defer tree.deinit();

    testing.expect(tree.nodes.len == 6);

    for (tree.nodes) |node| {
        if (node == ._return) {
            testing.expectEqualSlices(u8, node._return.token.literal, "return");
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
