const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;
const testing = std.testing;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const ast = @import("ast.zig");
const Node = ast.Node;
const Tree = ast.Tree;
const Errors = @import("error.zig").Errors;

//! This is where the `Parser` will parse all tokens, which are
//! parsed by the `Lexer`, into an AST tree.

/// Precendence represent the order of importance
/// The higher the value, the earlier it will be executed
/// This means a function call will be executed before a prefix,
/// and the product will be calculated before the sum.
///
/// Note that the order of these is important to determine the precedence in binary operations
const Precedence = enum(u4) {
    lowest,
    range,
    @"or",
    @"and",
    assign,
    equals,
    less_greater,
    bitwise_or,
    bitwise_xor,
    bitwise_and,
    shift,
    sum,
    product,
    prefix,
    call,
    index,

    /// Returns the integer value of the enum
    fn val(self: Precedence) u4 {
        return @enumToInt(self);
    }
};

/// Determines the Precendence based on the given Token Type
fn findPrecedence(token_type: Token.TokenType) Precedence {
    return switch (token_type) {
        .double_period => .range,
        .@"or" => .@"or",
        .@"and" => .@"and",
        .assign, .equal_add, .equal_sub, .equal_mul, .equal_div => .assign,
        .equal, .not_equal => .equals,
        .less_than, .greater_than, .less_than_equal, .greater_than_equal => .less_greater,
        .ampersand => .bitwise_and,
        .pipe => .bitwise_or,
        .caret => .bitwise_xor,
        .shift_left, .shift_right => .shift,
        .plus, .minus => .sum,
        .slash, .asterisk, .percent => .product,
        .left_parenthesis => .call,
        .left_bracket => .index,
        .period => .index,
        else => .lowest,
    };
}

/// Parses source code into an AST tree
pub fn parse(allocator: *Allocator, source: []const u8, _errors: *Errors) Parser.Error!*Tree {
    var lexer = Lexer.init(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var parser = Parser{
        .current_token = lexer.next(),
        .peek_token = lexer.next(),
        .allocator = &arena.allocator,
        .lexer = &lexer,
        .source = source,
        .err = _errors,
    };

    var nodes = ArrayList(Node).init(parser.allocator);
    errdefer nodes.deinit();

    while (!parser.currentIsType(.eof)) : (parser.next()) {
        try nodes.append(try parser.parseStatement());
    }

    const tree = try allocator.create(Tree);
    tree.* = Tree{
        .nodes = nodes.toOwnedSlice(),
        .arena = arena.state,
        .allocator = allocator,
    };

    return tree;
}

/// Parser retrieves tokens from our Lexer and turns them into
/// nodes to create an AST.
pub const Parser = struct {
    current_token: Token,
    peek_token: Token,
    allocator: *Allocator,
    lexer: *Lexer,
    source: []const u8,
    err: *Errors,

    pub const Error = error{
        ParserError,
        OutOfMemory,
        Overflow,
        InvalidCharacter,
    };

    /// Returns `Error.ParserError` and appends an error message to the `errors` list.
    fn fail(self: *Parser, msg: []const u8, index: usize) Error {
        try self.err.add(msg, index, .err);
        return Error.ParserError;
    }

    /// Sets the current token to the peek token and retrieves a new
    /// token from the Lexer and sets its value to the peak token.
    fn next(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.next();
    }

    /// Parses the statement into a node
    fn parseStatement(self: *Parser) Error!Node {
        return switch (self.current_token.token_type) {
            .comment => self.parseComment(),
            .constant, .mutable => self.parseDeclaration(),
            .@"return" => self.parseReturn(),
            .@"switch" => self.parseSwitchStatement(),
            else => self.parseExpressionStatement(),
        };
    }

    /// Parses a declaration
    fn parseDeclaration(self: *Parser) Error!Node {
        const decl = try self.allocator.create(Node.Declaration);
        decl.* = .{
            .token = self.current_token,
            .name = undefined,
            .value = undefined,
            .type_def = null,
            .mutable = self.current_token.token_type == .mutable,
        };

        try self.expectPeek(.identifier);

        decl.name = try self.parseIdentifier();

        if (self.peekIsType(.colon)) {
            self.next();
            self.next();
            decl.type_def = try self.parseTypeExpression();
        }

        try self.expectPeek(.assign);

        self.next();

        decl.value = try self.parseExpression(.lowest);

        return Node{ .declaration = decl };
    }

    /// Parses a return statement
    fn parseReturn(self: *Parser) Error!Node {
        const ret = try self.allocator.create(Node.Return);
        ret.* = .{ .token = self.current_token, .value = undefined };
        self.next();
        ret.value = try self.parseExpression(.lowest);
        return Node{ .@"return" = ret };
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
        statement.* = .{ .token = self.current_token, .value = try self.parseExpression(.lowest) };
        return Node{ .expression = statement };
    }

    /// Determines the correct expression type based on the current token type
    fn parseExpression(self: *Parser, prec: Precedence) Error!Node {
        var left = switch (self.current_token.token_type) {
            .identifier => try self.parseIdentifier(),
            .integer => try self.parseIntegerLiteral(),
            .string => try self.parseStringLiteral(),
            .bang => try self.parsePrefixExpression(),
            .minus => try self.parsePrefixExpression(),
            .tilde => try self.parsePrefixExpression(),
            .import => try self.parseImportExpression(),
            .@"true", .@"false" => try self.parseBoolean(),
            .@"if" => try self.parseIfExpression(),
            .left_parenthesis => try self.parseGroupedExpression(),
            .function => try self.parseFunctionLiteral(true),
            .left_bracket => try self.parseDataStructure(false),
            .while_loop => try self.parseWhile(),
            .for_loop => try self.parseFor(),
            .nil => try self.parseNil(),
            .@"break" => try self.parseBreak(),
            .@"continue" => try self.parseContinue(),
            .@"enum" => try self.parseEnum(),
            else => return self.fail("Unexpected token", self.current_token.start),
        };

        while (prec.val() < findPrecedence(self.peek_token.token_type).val()) {
            left = switch (self.peek_token.token_type) {
                .left_parenthesis => blk: {
                    self.next();
                    break :blk try self.parseCallExpression(left);
                },
                .left_bracket, .period => blk: {
                    self.next();
                    break :blk try self.parseIndexExpression(left);
                },
                .assign => blk: {
                    self.next();
                    break :blk try self.parseAssignment(left);
                },
                .double_period => blk: {
                    self.next();
                    break :blk try self.parseRange(left);
                },
                .plus,
                .minus,
                .slash,
                .asterisk,
                .percent,
                .ampersand,
                .pipe,
                .caret,
                .equal,
                .not_equal,
                .less_than,
                .greater_than,
                .less_than_equal,
                .greater_than_equal,
                .shift_left,
                .shift_right,
                .@"and",
                .@"or",
                .equal_add,
                .equal_sub,
                .equal_mul,
                .equal_div,
                => blk: {
                    self.next();
                    break :blk try self.parseInfixExpression(left);
                },
                else => return left,
            };
        }

        return left;
    }

    /// Parses the current token into a prefix, errors if current token is not a prefix token
    fn parsePrefixExpression(self: *Parser) Error!Node {
        const expression = try self.allocator.create(Node.Prefix);
        expression.* = .{
            .token = self.current_token,
            .operator = Node.Prefix.Op.fromToken(self.current_token),
            .right = undefined,
        };

        self.next();

        expression.right = try self.parseExpression(.prefix);

        return Node{ .prefix = expression };
    }

    /// Parses the current token into an infix expression
    fn parseInfixExpression(self: *Parser, left: Node) Error!Node {
        const expression = try self.allocator.create(Node.Infix);
        expression.* = .{
            .token = self.current_token,
            .operator = Node.Infix.Op.fromToken(self.current_token),
            .left = left,
            .right = undefined,
        };

        const prec = findPrecedence(self.current_token.token_type);
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

    /// Parses the current token into a string literal
    fn parseStringLiteral(self: *Parser) Error!Node {
        const literal = try self.allocator.create(Node.StringLiteral);
        const token = self.current_token;
        literal.* = .{ .token = token, .value = try self.allocator.dupe(u8, self.source[token.start..token.end]) };
        return Node{ .string_lit = literal };
    }

    /// Parses an expression into a boolean node
    fn parseBoolean(self: *Parser) !Node {
        const boolean = try self.allocator.create(Node.Bool);
        boolean.* = .{
            .token = self.current_token,
            .value = self.currentIsType(.@"true"),
        };
        return Node{ .boolean = boolean };
    }

    /// Parses expressions into a grouped expression
    fn parseGroupedExpression(self: *Parser) Error!Node {
        self.next();
        const exp = try self.parseExpression(.lowest);
        try self.expectPeek(.right_parenthesis);
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
        try self.expectPeek(.left_parenthesis);

        self.next();
        exp.condition = try self.parseExpression(.lowest);

        try self.expectPeek(.right_parenthesis);

        try self.expectPeek(.left_brace);

        exp.true_pong = try self.parseBlockStatement();

        if (self.peekIsType(.@"else")) {
            self.next();

            if (self.peekIsType(.@"if")) {
                self.next();
                exp.false_pong = try self.parseStatement();
                return Node{ .if_expression = exp };
            }

            try self.expectPeek(.left_brace);

            exp.false_pong = try self.parseBlockStatement();
        }

        return Node{ .if_expression = exp };
    }

    /// Parses the tokens into a `Node.BlockStatement` until a right brace is found
    /// Indirectly asserts a closing bracket is found
    /// Parse statements using this function do not have to assert for it.
    fn parseBlockStatement(self: *Parser) !Node {
        const block = try self.allocator.create(Node.BlockStatement);
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
    fn parseFunctionLiteral(self: *Parser, parse_body: bool) Error!Node {
        const func = try self.allocator.create(Node.FunctionLiteral);
        func.* = .{
            .token = self.current_token,
            .params = undefined,
            .body = null,
            .ret_type = undefined,
        };

        try self.expectPeek(.left_parenthesis);

        func.params = try self.parseFunctionParameters();

        self.next();
        func.ret_type = try self.parseTypeExpression();

        if (parse_body) {
            try self.expectPeek(.left_brace);
            func.body = try self.parseBlockStatement();
        }

        return Node{ .func_lit = func };
    }

    /// Parses the tokens into a list of nodes representing the parameters
    /// of a function literal.
    fn parseFunctionParameters(self: *Parser) Error![]Node {
        var list = ArrayList(Node).init(self.allocator);
        errdefer list.deinit();
        self.next();

        if (self.currentIsType(.right_parenthesis)) {
            return list.toOwnedSlice();
        }

        try list.append(try self.parseArgument());

        while (self.peekIsType(.comma)) {
            self.next();
            self.next();
            try list.append(try self.parseArgument());
        }

        try self.expectPeek(.right_parenthesis);

        return list.toOwnedSlice();
    }

    /// Parses an argument that contains an identifier Node and its type
    fn parseArgument(self: *Parser) Error!Node {
        const node = try self.allocator.create(Node.FunctionArgument);
        node.* = .{
            .token = self.current_token,
            .value = self.source[self.current_token.start..self.current_token.end],
            .arg_type = undefined,
        };
        try self.expectPeek(.colon);
        self.next();
        node.arg_type = try self.parseTypeExpression();

        return Node{ .func_arg = node };
    }

    /// Parses the current token into a call expression
    /// Accepts a function node
    fn parseCallExpression(self: *Parser, func: Node) Error!Node {
        const call = try self.allocator.create(Node.CallExpression);
        call.* = .{
            .token = self.current_token,
            .function = func,
            .arguments = try self.parseArguments(.right_parenthesis),
        };
        return Node{ .call_expression = call };
    }

    /// Parses the next set of tokens into argument nodes
    fn parseArguments(self: *Parser, comptime end_type: Token.TokenType) Error![]Node {
        var list = ArrayList(Node).init(self.allocator);
        errdefer list.deinit();

        // no arguments
        if (self.peekIsType(end_type)) {
            self.next();
            return list.toOwnedSlice();
        }

        self.next();
        try list.append(try self.parseExpression(.lowest));

        while (self.peekIsType(.comma)) {
            self.next();
            self.next();
            try list.append(try self.parseExpression(.lowest));
        }

        try self.expectPeek(end_type);

        return list.toOwnedSlice();
    }

    /// Parses the token into an `ArrayLiteral`
    fn parseDataStructure(self: *Parser, is_type_defintion: bool) Error!Node {
        const ds = try self.allocator.create(Node.DataStructure);
        ds.* = .{
            .token = self.current_token,
            .d_type = .array,
            .value = null,
            .type_def_key = undefined,
            .type_def_value = null,
            .len = null,
        };

        if (!self.peekIsType(.right_bracket)) {
            self.next();
            ds.len = try self.parseIntegerLiteral();
        }
        try self.expectPeek(.right_bracket);

        self.next();

        ds.type_def_key = try self.parseTypeExpression();
        if (self.peekIsType(.colon)) {
            self.next();
            self.next();
            ds.type_def_value = try self.parseTypeExpression();
            ds.d_type = .map;
        }

        // if this is a type declaration, i.e fn(x: []int) then don't parse arguments
        if (!is_type_defintion) {
            try self.expectPeek(.left_brace);

            ds.value = if (ds.d_type == .array)
                try self.parseArguments(.right_brace)
            else
                try self.parseMap();

            if (ds.len) |len|
                if (ds.value.?.len != len.int_lit.value)
                    return self.fail("Mismatching data structure size", self.current_token.start);
        }

        return Node{ .data_structure = ds };
    }

    /// Parses the map key:value pairs into a list of Nodes
    fn parseMap(self: *Parser) Error![]Node {
        var pairs = std.ArrayList(Node).init(self.allocator);

        while (!self.peekIsType(.right_brace)) {
            self.next();
            const pair = try self.parsePair();
            try pairs.append(pair);
            if (!self.peekIsType(.right_brace))
                try self.expectPeek(.comma);
        }

        try self.expectPeek(.right_brace);

        return pairs.toOwnedSlice();
    }

    /// Parses the next token into a Key Value `MapPair`
    fn parsePair(self: *Parser) Error!Node {
        const pair = try self.allocator.create(Node.MapPair);
        pair.* = .{ .token = self.current_token, .key = try self.parseExpression(.lowest), .value = undefined };

        try self.expectPeek(.colon);

        // skip over colon
        self.next();

        pair.value = try self.parseExpression(.lowest);
        return Node{ .map_pair = pair };
    }

    /// Parses the selector to retreive a value from an Array or Map
    fn parseIndexExpression(self: *Parser, left: Node) Error!Node {
        const token = self.current_token;
        const index = try self.allocator.create(Node.IndexExpression);
        index.* = .{ .token = token, .left = left, .index = undefined };
        self.next();

        index.index = if (token.token_type == .period) blk: {
            if (!self.currentIsType(.identifier))
                return self.fail("Expected identifier", self.current_token.start);
            break :blk try self.parseStringLiteral();
        } else
            try self.parseExpression(.lowest);

        if (token.token_type != .period) {
            try self.expectPeek(.right_bracket);
        }

        return Node{ .index = index };
    }

    /// Parses a while token into a while node
    fn parseWhile(self: *Parser) Error!Node {
        const node = try self.allocator.create(Node.WhileLoop);
        node.* = .{
            .token = self.current_token,
            .condition = undefined,
            .block = undefined,
        };

        try self.expectPeek(.left_parenthesis);

        // skip ( token
        self.next();

        node.condition = try self.parseExpression(.lowest);

        try self.expectPeek(.right_parenthesis);

        try self.expectPeek(.left_brace);

        // parseBlockStatement already asserts for a closing bracket, so return after parsing
        node.block = try self.parseBlockStatement();

        return Node{ .while_loop = node };
    }

    /// Parses a for expression
    fn parseFor(self: *Parser) Error!Node {
        const node = try self.allocator.create(Node.ForLoop);
        node.* = .{
            .token = self.current_token,
            .iter = undefined,
            .capture = undefined,
            .index = undefined,
            .block = undefined,
        };
        try self.expectPeek(.left_parenthesis);
        self.next();
        node.iter = try self.parseExpression(.lowest);

        try self.expectPeek(.right_parenthesis);
        try self.expectPeek(.pipe);
        self.next();

        node.capture = try self.parseIdentifier();

        // incase there's a 2nd capture for the index identifier i.e. |id, index|
        if (self.peekIsType(.comma)) {
            self.next();
            self.next();
            node.index = try self.parseIdentifier();
        }

        try self.expectPeek(.pipe);
        try self.expectPeek(.left_brace);

        node.block = try self.parseBlockStatement();

        return Node{ .for_loop = node };
    }

    /// Parses the current expression as an assignment. Compiler does checking for mutating
    fn parseAssignment(self: *Parser, left: Node) Error!Node {
        if (left != .identifier and left != .index)
            return self.fail("Expected identifier or index", self.current_token.start);

        const node = try self.allocator.create(Node.Assignment);
        node.* = .{
            .token = self.peek_token,
            .left = left,
            .right = undefined,
        };

        self.next();

        node.right = try self.parseExpression(.lowest);

        return Node{ .assignment = node };
    }

    /// Parses an expression into a `Node.Range` i.e. 0..5
    fn parseRange(self: *Parser, left: Node) Error!Node {
        const range = try self.allocator.create(Node.Range);
        range.* = .{
            .token = self.current_token,
            .left = left,
            .right = undefined,
        };

        self.next();

        range.right = try self.parseExpression(.lowest);

        return Node{ .range = range };
    }

    /// Parses a comment token into a `Comment` node
    fn parseComment(self: *Parser) Error!Node {
        const comment = try self.allocator.create(Node.Comment);
        const token = self.current_token;
        comment.* = .{
            .token = self.current_token,
            .value = self.source[token.start..token.end],
        };

        return Node{ .comment = comment };
    }

    /// Literal Nil value parsing
    fn parseNil(self: *Parser) Error!Node {
        const nil = try self.allocator.create(Node.Nil);
        nil.* = .{
            .token = self.current_token,
        };
        return Node{ .nil = nil };
    }

    /// Constructs a `Break` node
    fn parseBreak(self: *Parser) Error!Node {
        const node = try self.allocator.create(Node.Break);
        node.* = .{
            .token = self.current_token,
        };
        return Node{ .@"break" = node };
    }

    /// Constructs a `Continue` node
    fn parseContinue(self: *Parser) Error!Node {
        const node = try self.allocator.create(Node.Continue);
        node.* = .{
            .token = self.current_token,
        };
        return Node{ .@"continue" = node };
    }

    /// Parses an enum statement
    fn parseEnum(self: *Parser) Error!Node {
        const node = try self.allocator.create(Node.EnumLiteral);
        var enums = std.ArrayList(Node).init(self.allocator);
        node.* = .{
            .token = self.current_token,
            .nodes = undefined,
        };

        try self.expectPeek(.left_brace);
        self.next();
        try enums.append(try self.parseIdentifier());

        while (self.peekIsType(.comma)) {
            self.next();
            self.next();
            try enums.append(try self.parseIdentifier());
        }

        try self.expectPeek(.right_brace);

        node.nodes = enums.toOwnedSlice();

        return Node{ .@"enum" = node };
    }

    /// Parses the import expression i.e. const std = import("std")
    fn parseImportExpression(self: *Parser) Error!Node {
        const import = try self.allocator.create(Node.Import);
        import.* = .{ .token = self.current_token, .value = undefined };

        try self.expectPeek(.left_parenthesis);

        self.next();
        import.value = try self.parseExpression(.lowest);

        try self.expectPeek(.right_parenthesis);

        return Node{ .import = import };
    }

    /// Parses a switch statement into the `Switch` `Node`.
    fn parseSwitchStatement(self: *Parser) Error!Node {
        const node = try self.allocator.create(Node.SwitchLiteral);
        node.* = .{ .token = self.current_token, .capture = undefined, .prongs = undefined };

        try self.expectPeek(.left_parenthesis);

        self.next();
        node.capture = try self.parseExpression(.lowest);

        try self.expectPeek(.right_parenthesis);
        try self.expectPeek(.left_brace);

        self.next();
        var prongs = std.ArrayList(Node).init(self.allocator);

        try prongs.append(try self.parseSwitchProng());

        while (self.peekIsType(.comma)) {
            self.next();
            self.next();
            try prongs.append(try self.parseSwitchProng());
        }
        node.prongs = prongs.toOwnedSlice();

        try self.expectPeek(.right_brace);

        return Node{ .switch_statement = node };
    }

    /// Parses a switch prong i.e. x: 5 + 5 into a `Node.SwitchProng`
    fn parseSwitchProng(self: *Parser) Error!Node {
        const node = try self.allocator.create(Node.SwitchProng);
        node.* = .{ .token = undefined, .left = try self.parseExpression(.lowest), .right = undefined };

        try self.expectPeek(.colon);
        node.token = self.current_token;
        self.next();

        node.right = if (self.currentIsType(.left_brace))
            try self.parseBlockStatement()
        else
            try self.parseExpressionStatement();

        return Node{ .switch_prong = node };
    }

    /// Parses a type literal into a `Node.TypeDef` `Node`.
    /// i.e. parses const x: bool = true where 'bool' is the type expression
    fn parseTypeExpression(self: *Parser) Error!Node {
        const node = try self.allocator.create(Node.TypeDef);
        node.* = .{ .token = self.current_token, .value = null };

        switch (self.current_token.token_type) {
            .bool_type, .int_type, .string_type, .void_type => {},
            .left_bracket => node.value = try self.parseDataStructure(true),
            .function => node.value = try self.parseFunctionLiteral(false),
            else => return self.fail("Unexpected token, expected a type", self.current_token.start),
        }

        return Node{ .type_def = node };
    }

    /// Determines if the next token is the expected token or not.
    /// Incase the next token is the wanted token, retun true and retrieve next token.
    fn expectPeek(self: *Parser, comptime token_type: Token.TokenType) !void {
        if (self.peekIsType(token_type)) {
            self.next();
            return;
        }
        return self.fail(
            "Expected token '" ++ Token.string(token_type) ++ "'",
            self.peek_token.start,
        );
    }

    /// Helper function to check if the peek token is of given type
    fn peekIsType(self: Parser, token_type: Token.TokenType) bool {
        return self.peek_token.token_type == token_type;
    }

    /// Helper function to check if the current token is of given type
    fn currentIsType(self: Parser, token_type: Token.TokenType) bool {
        return self.current_token.token_type == token_type;
    }
};

test "Parse Delcaration" {
    var allocator = testing.allocator;
    const test_cases = .{
        .{ .input = "const x = 5", .id = "x", .expected = 5, .mutable = false },
        .{ .input = "mut y = 50", .id = "y", .expected = 50, .mutable = true },
        .{ .input = "mut x = 2 const y = 5", .id = "y", .expected = 5, .mutable = false },
    };

    inline for (test_cases) |case| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = try parse(allocator, case.input, &errors);
        defer tree.deinit();
        const node = tree.nodes[tree.nodes.len - 1].declaration;
        testing.expectEqualSlices(u8, case.id, node.name.identifier.value);
        testing.expect(case.expected == node.value.int_lit.value);
        testing.expect(case.mutable == node.mutable);
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
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = try parse(allocator, case.input, &errors);
        defer tree.deinit();

        testing.expect(tree.nodes.len == 1);

        const node = tree.nodes[0].@"return".value;

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
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const identifier = tree.nodes[0].expression.value.identifier;
    testing.expect(identifier.token.token_type == .identifier);
    testing.expectEqualSlices(u8, identifier.value, input);
}

test "Parse integer literal" {
    const input = "124";
    var allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const literal = tree.nodes[0].expression.value.int_lit;
    testing.expect(literal.token.token_type == .integer);
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
        operator: Node.Prefix.Op,
        expected: VarValue,
    };
    const test_cases = &[_]TestCase{
        .{ .input = "-5", .operator = .minus, .expected = VarValue{ .int = 5 } },
        .{ .input = "!25", .operator = .bang, .expected = VarValue{ .int = 25 } },
        .{ .input = "!foobar", .operator = .bang, .expected = VarValue{ .string = "foobar" } },
        .{ .input = "-foobar", .operator = .minus, .expected = VarValue{ .string = "foobar" } },
        .{ .input = "!true", .operator = .bang, .expected = VarValue{ .boolean = true } },
        .{ .input = "!false", .operator = .bang, .expected = VarValue{ .boolean = false } },
    };

    const allocator = testing.allocator;
    for (test_cases) |case| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = try parse(allocator, case.input, &errors);
        defer tree.deinit();

        testing.expect(tree.nodes.len == 1);

        const prefix = tree.nodes[0].expression.value.prefix;
        switch (prefix.right) {
            .int_lit => |int| testing.expect(case.expected.int == int.value),
            .identifier => |id| testing.expectEqualSlices(u8, case.expected.string, id.value),
            .boolean => |boolean| testing.expect(case.expected.boolean == boolean.value),
            else => @panic("Unexpected Node"),
        }

        testing.expectEqual(case.operator, prefix.operator);
    }
}

test "Parse infix expressions - integer" {
    const TestCase = struct {
        input: []const u8,
        left: usize,
        operator: Node.Infix.Op,
        right: usize,
    };

    const test_cases = &[_]TestCase{
        .{ .input = "10 + 10", .left = 10, .operator = .add, .right = 10 },
        .{ .input = "10 - 10", .left = 10, .operator = .sub, .right = 10 },
        .{ .input = "10 * 10", .left = 10, .operator = .multiply, .right = 10 },
        .{ .input = "10 / 10", .left = 10, .operator = .divide, .right = 10 },
        .{ .input = "10 > 10", .left = 10, .operator = .greater_than, .right = 10 },
        .{ .input = "10 < 10", .left = 10, .operator = .less_than, .right = 10 },
        .{ .input = "10 == 10", .left = 10, .operator = .equal, .right = 10 },
        .{ .input = "10 != 10", .left = 10, .operator = .not_equal, .right = 10 },
    };

    const allocator = testing.allocator;
    for (test_cases) |case| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = try parse(allocator, case.input, &errors);
        defer tree.deinit();

        testing.expect(tree.nodes.len == 1);
        const node: Node = tree.nodes[0];
        const infix: *Node.Infix = node.expression.value.infix;
        testing.expectEqual(case.operator, infix.operator);
        testing.expectEqual(case.left, infix.left.int_lit.value);
        testing.expectEqual(case.right, infix.right.int_lit.value);
    }
}

test "Parse infix expressions - identifier" {
    const allocator = testing.allocator;
    const input = "foobar + foobarz";
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const node: Node = tree.nodes[0];
    const infix: *Node.Infix = node.expression.value.infix;
    testing.expectEqualSlices(u8, "foobar", infix.left.identifier.value);
    testing.expectEqualSlices(u8, "foobarz", infix.right.identifier.value);
    testing.expect(infix.operator == .add);
}

test "Parse infix expressions - boolean" {
    const allocator = testing.allocator;
    const input = "true == true";
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const node: Node = tree.nodes[0];
    const infix: *Node.Infix = node.expression.value.infix;
    testing.expectEqual(true, infix.left.boolean.value);
    testing.expectEqual(true, infix.right.boolean.value);
    testing.expect(infix.operator == .equal);
}

test "Boolean expression" {
    const allocator = testing.allocator;
    const input = "true";
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    testing.expect(tree.nodes[0].expression.value == .boolean);
}

test "If expression" {
    const allocator = testing.allocator;
    const input = "if (x < y) { x }";
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const if_exp = tree.nodes[0].expression.value.if_expression;
    testing.expect(if_exp.true_pong.block_statement.nodes[0] == .expression);
    testing.expectEqualSlices(
        u8,
        if_exp.true_pong.block_statement.nodes[0].expression.value.identifier.value,
        "x",
    );
}

test "If else expression" {
    const allocator = testing.allocator;
    const input = "if (x < y) { x } else { y }";
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const if_exp = tree.nodes[0].expression.value.if_expression;
    testing.expect(if_exp.true_pong.block_statement.nodes[0] == .expression);
    testing.expectEqualSlices(
        u8,
        if_exp.true_pong.block_statement.nodes[0].expression.value.identifier.value,
        "x",
    );
    testing.expect(if_exp.false_pong != null);
    testing.expect(if_exp.false_pong.?.block_statement.nodes[0] == .expression);
    testing.expectEqualSlices(
        u8,
        if_exp.false_pong.?.block_statement.nodes[0].expression.value.identifier.value,
        "y",
    );
}

test "If else-if expression" {
    const allocator = testing.allocator;
    const input = "if (x < y) { x } else if(x == 0) { y } else { z }";
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);
    const if_exp = tree.nodes[0].expression.value.if_expression;
    testing.expect(if_exp.true_pong.block_statement.nodes[0] == .expression);
    testing.expectEqualSlices(
        u8,
        if_exp.true_pong.block_statement.nodes[0].expression.value.identifier.value,
        "x",
    );
    testing.expect(if_exp.false_pong != null);
    testing.expect(if_exp.false_pong.?.expression.value == .if_expression);
    testing.expect(if_exp.false_pong.?.expression.value.if_expression.false_pong != null);
}

test "Function literal" {
    const input = "fn(x: int, y: int) fn(x: int) int { x + y }";
    const allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = parse(allocator, input, &errors) catch |err| {
        try errors.write(input, std.io.getStdErr().writer());
        return err;
    };
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const func = tree.nodes[0].expression.value.func_lit;
    testing.expect(func.params.len == 2);

    testing.expectEqualSlices(u8, func.params[0].func_arg.value, "x");
    testing.expectEqualSlices(u8, func.params[1].func_arg.value, "y");

    const body = func.body.?.block_statement.nodes[0];
    const infix = body.expression.value.infix;
    testing.expectEqual(infix.operator, .add);
    testing.expectEqualSlices(u8, infix.left.identifier.value, "x");
    testing.expectEqualSlices(u8, infix.right.identifier.value, "y");
}

test "Function parameters" {
    const test_cases = .{
        .{ .input = "fn() void {}", .expected = &[_][]const u8{} },
        .{ .input = "fn(x: int) void {}", .expected = &[_][]const u8{"x"} },
        .{ .input = "fn(x: int, y: int, z: int) void {}", .expected = &[_][]const u8{ "x", "y", "z" } },
    };
    const allocator = testing.allocator;
    inline for (test_cases) |case| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = try parse(allocator, case.input, &errors);
        defer tree.deinit();

        testing.expect(tree.nodes.len == 1);

        const func = tree.nodes[0].expression.value.func_lit;
        testing.expect(func.params.len == case.expected.len);

        inline for (case.expected) |exp, i| {
            testing.expectEqualSlices(u8, exp, func.params[i].func_arg.value);
        }
    }
}

test "Call expression" {
    const input = "add(1, 2 * 3, 4 + 5)";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const call = tree.nodes[0].expression.value.call_expression;
    testing.expectEqualSlices(u8, call.function.identifier.value, "add");
    testing.expect(call.arguments.len == 3);

    testing.expect(call.arguments[0].int_lit.value == 1);
    testing.expectEqual(call.arguments[1].infix.operator, .multiply);
    testing.expect(call.arguments[2].infix.right.int_lit.value == 5);
}

test "String expression" {
    const input = "\"Hello, world\"";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const string = tree.nodes[0].expression.value.string_lit;
    testing.expectEqualSlices(u8, "Hello, world", string.value);
}

test "Member expression" {
    const input = "foo.bar";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const index = tree.nodes[0].expression.value.index;
    testing.expectEqualSlices(u8, "bar", index.index.string_lit.value);
    testing.expectEqualSlices(u8, "foo", index.left.identifier.value);
    testing.expect(index.token.token_type == .period);
}

test "Array literal" {
    const input = "[]int{1, 2 * 2, 3 + 3}";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const array = tree.nodes[0].expression.value.data_structure;
    testing.expect(array.value.?.len == 3);
    testing.expect(array.value.?[0].int_lit.value == 1);
    testing.expect(array.value.?[1].infix.operator == .multiply);
}

test "Array index" {
    const input = "array[1]";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const index = tree.nodes[0].expression.value.index;
    testing.expect(index.left == .identifier);
    testing.expect(index.index.int_lit.value == 1);
}

test "Map Literal" {
    const Type = @import("value.zig").Type;
    const input = "[]string:int{\"foo\": 1, \"bar\": 5}";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const map = tree.nodes[0].expression.value.data_structure;
    testing.expect(map.value.?.len == 2);

    const expected = .{
        .{ .key = "foo", .value = 1 },
        .{ .key = "bar", .value = 5 },
    };

    inline for (expected) |case, i| {
        const pair: *Node.MapPair = map.value.?[i].map_pair;
        testing.expectEqualSlices(u8, case.key, pair.key.string_lit.value);
        testing.expect(case.value == pair.value.int_lit.value);
    }

    testing.expectEqual(Type.map, tree.nodes[0].getType().?);
    testing.expectEqual(Type.string, tree.nodes[0].getInnerType().?);
    testing.expectEqual(Type.integer, map.type_def_value.?.getInnerType().?);
}

test "While loop" {
    const input = "while(x < y) { x }";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const loop = tree.nodes[0].expression.value.while_loop;

    testing.expect(loop.condition.infix.operator == .less_than);
    testing.expect(loop.block.block_statement.nodes.len == 1);
    testing.expectEqualStrings("x", loop.block.block_statement.nodes[0].expression.value.identifier.value);
}

test "Assignment" {
    var allocator = testing.allocator;
    const test_cases = .{
        .{ .input = "x = 5", .id = "x", .expected = 5 },
        .{ .input = "y = 50", .id = "y", .expected = 50 },
        .{ .input = "x = 2 y = 5", .id = "y", .expected = 5 },
    };

    inline for (test_cases) |case, i| {
        var errors = Errors.init(allocator);
        defer errors.deinit();
        const tree = try parse(allocator, case.input, &errors);
        defer tree.deinit();
        const node = tree.nodes[tree.nodes.len - 1].expression.value.assignment;
        testing.expectEqualSlices(u8, case.id, node.left.identifier.value);
        testing.expect(case.expected == node.right.int_lit.value);
    }
}

test "Comment expression" {
    const input = "//This is a comment";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const comment = tree.nodes[0].comment;
    testing.expectEqualStrings("This is a comment", comment.value);
}

test "For loop" {
    const input = "for(x)|id,i|{ id }";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const loop = tree.nodes[0].expression.value.for_loop;

    testing.expect(loop.index != null);
    testing.expectEqualStrings("x", loop.iter.identifier.value);
    testing.expectEqualStrings("id", loop.capture.identifier.value);
    testing.expectEqualStrings("i", loop.index.?.identifier.value);
    testing.expect(loop.block.block_statement.nodes.len == 1);
    testing.expectEqualStrings("id", loop.block.block_statement.nodes[0].expression.value.identifier.value);
}

test "Enum" {
    const input = "enum{value, another_value, third_value }";
    const allocator = testing.allocator;

    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const enum_val = tree.nodes[0].expression.value.@"enum";
    testing.expect(enum_val.nodes.len == 3);
    testing.expectEqualStrings("value", enum_val.nodes[0].identifier.value);
    testing.expectEqualStrings("another_value", enum_val.nodes[1].identifier.value);
    testing.expectEqualStrings("third_value", enum_val.nodes[2].identifier.value);
}

test "Enum" {
    const input =
        \\switch(5){
        \\  1: 1 + 1,
        \\  2: {
        \\          if (true) {
        \\              1 + 2
        \\          } 
        \\      },
        \\  3: 2 + 2
        \\}
    ;
    const allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();
    const tree = try parse(allocator, input, &errors);
    defer tree.deinit();

    testing.expect(tree.nodes.len == 1);

    const switch_stmt = tree.nodes[0].switch_statement;
    testing.expect(switch_stmt.capture == .int_lit);
    testing.expect(switch_stmt.prongs.len == 3);
    for (switch_stmt.prongs) |p, i| {
        testing.expectEqual(@as(u64, i + 1), p.switch_prong.left.int_lit.value);
    }
    testing.expect(switch_stmt.prongs[1].switch_prong.right == .block_statement);
}

test "Type definitions" {
    const Type = @import("value.zig").Type;

    const cases = [_][]const u8{
        "fn(x: int, y: int)void{}",
        "const x: int = 10",
        "fn(x: []int)fn()string{}",
    };

    const allocator = testing.allocator;
    var errors = Errors.init(allocator);
    defer errors.deinit();

    const function = try parse(allocator, cases[0], &errors);
    defer function.deinit();

    const func_type = function.nodes[0].getInnerType();
    const arg_type = function.nodes[0].expression.value.func_lit.params[0].getType();
    testing.expectEqual(Type._void, func_type.?);
    testing.expectEqual(Type.integer, arg_type.?);

    const declaration = try parse(allocator, cases[1], &errors);
    defer declaration.deinit();

    const decl_type = declaration.nodes[0].getType();
    testing.expectEqual(Type.integer, decl_type.?);

    const array = try parse(allocator, cases[2], &errors);
    defer array.deinit();

    const array_type = array.nodes[0].expression.value.func_lit.params[0].getType();
    const scalar_type = array.nodes[0].expression.value.func_lit.params[0].getInnerType();
    const ret_type = array.nodes[0].getInnerType();
    testing.expectEqual(Type.list, array_type.?);
    testing.expectEqual(Type.integer, scalar_type.?);
    testing.expectEqual(Type.string, ret_type.?);
}
