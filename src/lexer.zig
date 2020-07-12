const std = @import("std");
const testing = std.testing;
const token = @import("token.zig");
const Token = token.Token;

/// Lexer reads the source code and turns it into tokens
pub const Lexer = struct {
    source: []const u8,
    position: usize = 0,
    read_position: usize = 0,
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

        const char = (&self.char).*;
        const token_type: Token.TokenType = switch (self.char) {
            '=' => if (self.peekChar() == '=') {
                self.readChar();
                self.readChar(); // So we don't read '=' token again
                return Token{ .type = .equal, .literal = "==" };
            } else .assign,
            '(' => .left_parenthesis,
            ')' => .right_parenthesis,
            ',' => .comma,
            '+' => .plus,
            '-' => .minus,
            '!' => if (self.peekChar() == '=') {
                self.readChar();
                self.readChar(); // So we don't read '=' token again
                return Token{ .type = .not_equal, .literal = "!=" };
            } else .illegal,
            '/' => .slash,
            '*' => .asterisk,
            '<' => .less_than,
            '>' => .greater_than,
            '{' => .left_brace,
            '}' => .right_brace,
            0 => .eof,
            else => |c| if (isLetter(c)) {
                const ident = self.readIdentifier();
                return Token{ .type = token.findType(ident), .literal = ident };
            } else if (isDigit(c)) {
                return Token{ .type = .integer, .literal = self.readNumber() };
            } else .illegal,
        };

        const literal = &[_]u8{char};

        // read the next character so we don't read our last character
        // again when nexToken is called.
        self.readChar();

        return Token{
            .type = token_type,
            .literal = literal,
        };
    }

    /// Tokenizes all tokens found in the input source
    pub fn tokenize(self: *Lexer, allocator: *std.mem.Allocator) ![]const Token {
        var token_list = std.ArrayList(Token).init(allocator);
        while (true) {
            const tok: *Token = try token_list.addOne();
            tok.* = self.next();
            if (tok.type == .eof) {
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

    /// Returns the character but does not increase the position
    fn peekChar(self: Lexer) u8 {
        return if (self.read_position >= self.source.len)
            0
        else
            self.source[self.read_position];
    }

    /// Skips whitespace until a character is found
    fn skipWhitespace(self: *Lexer) void {
        while (isWhitespace(self.char)) {
            self.readChar();
        }
    }

    /// Reads the next characters as identifier
    fn readIdentifier(self: *Lexer) []const u8 {
        const pos = self.position;
        while (isLetter(self.char)) {
            self.readChar();
        }
        return self.source[pos..self.position];
    }

    /// Reads the next characters as number
    fn readNumber(self: *Lexer) []const u8 {
        const pos = self.position;
        while (isDigit(self.char)) {
            self.readChar();
        }
        return self.source[pos..self.position];
    }
};

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
    ;

    const tests = &[_]Token{
        .{ .type = .constant, .literal = "const" },
        .{ .type = .identifier, .literal = "five" },
        .{ .type = .assign, .literal = "=" },
        .{ .type = .integer, .literal = "5" },
        .{ .type = .constant, .literal = "const" },
        .{ .type = .identifier, .literal = "ten" },
        .{ .type = .assign, .literal = "=" },
        .{ .type = .integer, .literal = "10" },
        .{ .type = .constant, .literal = "const" },
        .{ .type = .identifier, .literal = "add" },
        .{ .type = .assign, .literal = "=" },
        .{ .type = .function, .literal = "fn" },
        .{ .type = .left_parenthesis, .literal = "(" },
        .{ .type = .identifier, .literal = "x" },
        .{ .type = .comma, .literal = "," },
        .{ .type = .identifier, .literal = "y" },
        .{ .type = .right_parenthesis, .literal = ")" },
        .{ .type = .left_brace, .literal = "{" },
        .{ .type = .identifier, .literal = "x" },
        .{ .type = .plus, .literal = "+" },
        .{ .type = .identifier, .literal = "y" },
        .{ .type = .right_brace, .literal = "}" },
        .{ .type = .constant, .literal = "const" },
        .{ .type = .identifier, .literal = "result" },
        .{ .type = .assign, .literal = "=" },
        .{ .type = .identifier, .literal = "add" },
        .{ .type = .left_parenthesis, .literal = "(" },
        .{ .type = .identifier, .literal = "five" },
        .{ .type = .comma, .literal = "," },
        .{ .type = .identifier, .literal = "ten" },
        .{ .type = .right_parenthesis, .literal = ")" },
        .{ .type = .illegal, .literal = "!" },
        .{ .type = .minus, .literal = "-" },
        .{ .type = .slash, .literal = "/" },
        .{ .type = .asterisk, .literal = "*" },
        .{ .type = .integer, .literal = "5" },
        .{ .type = .integer, .literal = "5" },
        .{ .type = .less_than, .literal = "<" },
        .{ .type = .integer, .literal = "10" },
        .{ .type = .greater_than, .literal = ">" },
        .{ .type = .integer, .literal = "5" },
        .{ .type = ._if, .literal = "if" },
        .{ .type = .left_parenthesis, .literal = "(" },
        .{ .type = .integer, .literal = "5" },
        .{ .type = .less_than, .literal = "<" },
        .{ .type = .integer, .literal = "10" },
        .{ .type = .right_parenthesis, .literal = ")" },
        .{ .type = .left_brace, .literal = "{" },
        .{ .type = ._return, .literal = "return" },
        .{ .type = ._true, .literal = "true" },
        .{ .type = .right_brace, .literal = "}" },
        .{ .type = ._else, .literal = "else" },
        .{ .type = .left_brace, .literal = "{" },
        .{ .type = ._return, .literal = "return" },
        .{ .type = ._false, .literal = "false" },
        .{ .type = .right_brace, .literal = "}" },
        .{ .type = .integer, .literal = "10" },
        .{ .type = .equal, .literal = "==" },
        .{ .type = .integer, .literal = "10" },
        .{ .type = .integer, .literal = "10" },
        .{ .type = .not_equal, .literal = "!=" },
        .{ .type = .integer, .literal = "9" },
    };

    var lexer = Lexer.init(input);

    for (tests) |unit| {
        const current_token = lexer.next();

        testing.expectEqualSlices(u8, unit.literal, current_token.literal);
        testing.expectEqual(unit.type, current_token.type);
    }
}
