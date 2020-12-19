const std = @import("std");
const testing = std.testing;
const Token = @import("Token.zig");

//! The `Lexer` parses the source code into Tokens.
//! The definition of all tokens can be found in token.zig

/// Lexer reads the source code and turns it into tokens
const Lexer = @This();

/// Source code that is being tokenized
source: []const u8,
/// Current position in the source
position: usize = 0,
/// The position last read
read_position: usize = 0,
/// The current character that is being read
char: u8 = 0,

/// Creates a new lexer using the given source code
pub fn init(source: []const u8) Lexer {
    var lexer = Lexer{ .source = source };
    lexer.readChar();
    return lexer;
}

/// Parses the source and returns the next token found
pub fn next(self: *Lexer) Token {
    self.skipWhitespace();

    const token_type: Token.TokenType = switch (self.char) {
        '=' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar(); // So we don't read '=' token again
            return Token{ .token_type = .equal, .start = start, .end = self.position };
        } else .assign,
        '(' => .left_parenthesis,
        ')' => .right_parenthesis,
        ',' => .comma,
        '+' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .equal_add, .start = start, .end = self.position };
        } else .plus,
        '-' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .equal_sub, .start = start, .end = self.position };
        } else .minus,
        '!' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar(); // So we don't read '=' token again
            return Token{ .token_type = .not_equal, .start = start, .end = self.position };
        } else .bang,
        '/' => if (self.peekChar() == '/') {
            const start = self.position;
            self.readLine();
            return Token{ .token_type = .comment, .start = start + 2, .end = self.position };
        } else if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .equal_div, .start = start, .end = self.position };
        } else .slash,
        '*' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .equal_mul, .start = start, .end = self.position };
        } else .asterisk,
        '^' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .equal_caret, .start = start, .end = self.position };
        } else .caret,
        '&' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .equal_ampersand, .start = start, .end = self.position };
        } else .ampersand,
        '|' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .equal_pipe, .start = start, .end = self.position };
        } else .pipe,
        '<' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .less_than_equal, .start = start, .end = self.position };
        } else if (self.peekChar() == '<') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .shift_left, .start = start, .end = self.position };
        } else .less_than,
        '>' => if (self.peekChar() == '=') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .greater_than_equal, .start = start, .end = self.position };
        } else if (self.peekChar() == '>') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .shift_right, .start = start, .end = self.position };
        } else .greater_than,
        '{' => .left_brace,
        '}' => .right_brace,
        '.' => if (self.peekChar() == '.') {
            const start = self.position;
            self.readChar();
            self.readChar();
            return Token{ .token_type = .double_period, .start = start, .end = self.position };
        } else .period,
        '[' => .left_bracket,
        ']' => .right_bracket,
        ':' => .colon,
        '"' => {
            self.readChar();
            const start = self.position;
            self.readString();
            defer self.readChar();
            return Token{ .token_type = .string, .start = start, .end = self.position };
        },
        '%' => .percent,
        '~' => .tilde,
        '?' => .query,
        0 => .eof,
        else => |c| if (isLetter(c)) {
            const start = self.position;
            const ident = self.readIdentifier();
            return Token{ .token_type = Token.findType(ident), .start = start, .end = self.position };
        } else if (isDigit(c)) {
            const start = self.position;
            self.readNumber();
            return Token{ .token_type = .integer, .start = start, .end = self.position };
        } else .illegal,
    };

    // read the next character so we don't read our last character
    // again when nexToken is called.
    defer self.readChar();

    return Token{
        .token_type = token_type,
        .start = self.position,
        .end = self.read_position,
    };
}

/// Tokenizes all tokens found in the input source and returns a list of tokens.
/// Memory is owned by the caller.
pub fn tokenize(self: *Lexer, allocator: *std.mem.Allocator) ![]const Token {
    var token_list = std.ArrayList(Token).init(allocator);
    while (true) {
        const tok: *Token = try token_list.addOne();
        tok.* = self.next();
        if (tok.token_type == .eof) {
            return token_list.toOwnedSlice();
        }
    }
}

/// Reads exactly one character
fn readChar(self: *Lexer) void {
    self.char = if (self.read_position >= self.source.len) 0 else self.source[self.read_position];
    self.position = self.read_position;
    self.read_position += 1;
}

/// Returns the next character but does not increase the Lexer's position
fn peekChar(self: Lexer) u8 {
    return if (self.read_position >= self.source.len)
        0
    else
        self.source[self.read_position];
}

/// Skips whitespace until a non-whitespace character is found
fn skipWhitespace(self: *Lexer) void {
    while (isWhitespace(self.char)) {
        self.readChar();
    }
}

/// Reads the next characters as identifier and returns the identifier
fn readIdentifier(self: *Lexer) []const u8 {
    const pos = self.position;
    while (isLetter(self.char)) {
        self.readChar();
    }
    return self.source[pos..self.position];
}

/// Reads the next characters as number
fn readNumber(self: *Lexer) void {
    while (isDigit(self.char))
        self.readChar();
}

/// Reads a string from the current character
fn readString(self: *Lexer) void {
    while (self.char != '"' and self.char != 0)
        self.readChar();
}

/// Reads until the end of the line or EOF
fn readLine(self: *Lexer) void {
    while (self.char != '\n' and self.char != 0)
        self.readChar();
}

/// Returns true if the given character is considered whitespace
fn isWhitespace(char: u8) bool {
    return switch (char) {
        ' ', '\t', '\n', '\r' => true,
        else => false,
    };
}

/// Returns true if the given character is a digit
fn isDigit(char: u8) bool {
    return switch (char) {
        '0'...'9' => true,
        else => false,
    };
}

/// Returns true if the given character is a letter
fn isLetter(char: u8) bool {
    return switch (char) {
        'a'...'z', 'A'...'Z', '_' => true,
        else => false,
    };
}

test "All supported tokens" {
    const input =
        \\const five = 5
        \\const ten = 10
        \\const add = fn(x, y) {
        \\  x + y    
        \\}
        \\
        \\const result = add(five, ten)
        \\!-/*5
        \\5 < 10 > 5
        \\
        \\if (5 < 10) {
        \\  return true
        \\} else {
        \\  return false
        \\}
        \\
        \\10 == 10
        \\10 != 9
        \\"foo"
        \\"foo bar"
        \\"foo".len
        \\[1, 2]
        \\{"key":1}
        \\//this is a comment
        \\nil
    ;

    const tests = &[_]Token{
        .{ .token_type = .constant, .start = 0, .end = 5 },
        .{ .token_type = .identifier, .start = 6, .end = 10 },
        .{ .token_type = .assign, .start = 11, .end = 12 },
        .{ .token_type = .integer, .start = 13, .end = 14 },
        .{ .token_type = .constant, .start = 15, .end = 20 },
        .{ .token_type = .identifier, .start = 21, .end = 24 },
        .{ .token_type = .assign, .start = 25, .end = 26 },
        .{ .token_type = .integer, .start = 27, .end = 29 },
        .{ .token_type = .constant, .start = 30, .end = 35 },
        .{ .token_type = .identifier, .start = 36, .end = 39 },
        .{ .token_type = .assign, .start = 40, .end = 41 },
        .{ .token_type = .function, .start = 42, .end = 44 },
        .{ .token_type = .left_parenthesis, .start = 44, .end = 45 },
        .{ .token_type = .identifier, .start = 45, .end = 46 },
        .{ .token_type = .comma, .start = 46, .end = 47 },
        .{ .token_type = .identifier, .start = 48, .end = 49 },
        .{ .token_type = .right_parenthesis, .start = 49, .end = 50 },
        .{ .token_type = .left_brace, .start = 51, .end = 52 },
        .{ .token_type = .identifier, .start = 55, .end = 56 },
        .{ .token_type = .plus, .start = 57, .end = 58 },
        .{ .token_type = .identifier, .start = 59, .end = 60 },
        .{ .token_type = .right_brace, .start = 65, .end = 66 },
        .{ .token_type = .constant, .start = 68, .end = 73 },
        .{ .token_type = .identifier, .start = 74, .end = 80 },
        .{ .token_type = .assign, .start = 81, .end = 82 },
        .{ .token_type = .identifier, .start = 83, .end = 86 },
        .{ .token_type = .left_parenthesis, .start = 86, .end = 87 },
        .{ .token_type = .identifier, .start = 87, .end = 91 },
        .{ .token_type = .comma, .start = 91, .end = 92 },
        .{ .token_type = .identifier, .start = 93, .end = 96 },
        .{ .token_type = .right_parenthesis, .start = 96, .end = 97 },
        .{ .token_type = .bang, .start = 98, .end = 99 },
        .{ .token_type = .minus, .start = 99, .end = 100 },
        .{ .token_type = .slash, .start = 100, .end = 101 },
        .{ .token_type = .asterisk, .start = 101, .end = 102 },
        .{ .token_type = .integer, .start = 102, .end = 103 },
        .{ .token_type = .integer, .start = 104, .end = 105 },
        .{ .token_type = .less_than, .start = 106, .end = 107 },
        .{ .token_type = .integer, .start = 108, .end = 110 },
        .{ .token_type = .greater_than, .start = 111, .end = 112 },
        .{ .token_type = .integer, .start = 113, .end = 114 },
        .{ .token_type = .@"if", .start = 116, .end = 118 },
        .{ .token_type = .left_parenthesis, .start = 119, .end = 120 },
        .{ .token_type = .integer, .start = 120, .end = 121 },
        .{ .token_type = .less_than, .start = 122, .end = 123 },
        .{ .token_type = .integer, .start = 124, .end = 126 },
        .{ .token_type = .right_parenthesis, .start = 126, .end = 127 },
        .{ .token_type = .left_brace, .start = 128, .end = 129 },
        .{ .token_type = .@"return", .start = 132, .end = 138 },
        .{ .token_type = .@"true", .start = 139, .end = 143 },
        .{ .token_type = .right_brace, .start = 144, .end = 145 },
        .{ .token_type = .@"else", .start = 146, .end = 150 },
        .{ .token_type = .left_brace, .start = 151, .end = 152 },
        .{ .token_type = .@"return", .start = 155, .end = 161 },
        .{ .token_type = .@"false", .start = 162, .end = 167 },
        .{ .token_type = .right_brace, .start = 168, .end = 169 },
        .{ .token_type = .integer, .start = 171, .end = 173 },
        .{ .token_type = .equal, .start = 174, .end = 176 },
        .{ .token_type = .integer, .start = 177, .end = 179 },
        .{ .token_type = .integer, .start = 180, .end = 182 },
        .{ .token_type = .not_equal, .start = 183, .end = 185 },
        .{ .token_type = .integer, .start = 186, .end = 187 },
        .{ .token_type = .string, .start = 189, .end = 192 },
        .{ .token_type = .string, .start = 195, .end = 202 },
        .{ .token_type = .string, .start = 205, .end = 208 },
        .{ .token_type = .period, .start = 209, .end = 210 },
        .{ .token_type = .identifier, .start = 210, .end = 213 },
        .{ .token_type = .left_bracket, .start = 214, .end = 215 },
        .{ .token_type = .integer, .start = 215, .end = 216 },
        .{ .token_type = .comma, .start = 216, .end = 217 },
        .{ .token_type = .integer, .start = 218, .end = 219 },
        .{ .token_type = .right_bracket, .start = 219, .end = 220 },
        .{ .token_type = .left_brace, .start = 221, .end = 222 },
        .{ .token_type = .string, .start = 223, .end = 226 },
        .{ .token_type = .colon, .start = 227, .end = 228 },
        .{ .token_type = .integer, .start = 228, .end = 229 },
        .{ .token_type = .right_brace, .start = 229, .end = 230 },
        .{ .token_type = .comment, .start = 233, .end = 250 },
        .{ .token_type = .nil, .start = 251, .end = 254 },
        .{ .token_type = .eof, .start = 254, .end = 255 },
    };

    var lexer = Lexer.init(input);

    for (tests) |unit| {
        const current_token = lexer.next();

        testing.expectEqual(unit.start, current_token.start);
        testing.expectEqual(unit.end, current_token.end);
        testing.expectEqual(unit.token_type, current_token.token_type);
    }
}
