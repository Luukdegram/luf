const std = @import("std");
const testing = std.testing;
const token = @import("token.zig");
const Token = token.Token;

//! The `Lexer` parses the source code into Tokens.
//! The definition of all tokens can be found in token.zig

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

        const token_type: Token.TokenType = switch (self.char) {
            '=' => if (self.peekChar() == '=') {
                const start = self.position;
                self.readChar();
                self.readChar(); // So we don't read '=' token again
                return Token{ .type = .equal, .start = start, .end = self.position };
            } else .assign,
            '(' => .left_parenthesis,
            ')' => .right_parenthesis,
            ',' => .comma,
            '+' => .plus,
            '-' => .minus,
            '!' => if (self.peekChar() == '=') {
                const start = self.position;
                self.readChar();
                self.readChar(); // So we don't read '=' token again
                return Token{ .type = .not_equal, .start = start, .end = self.position };
            } else .bang,
            '/' => .slash,
            '*' => .asterisk,
            '<' => .less_than,
            '>' => .greater_than,
            '{' => .left_brace,
            '}' => .right_brace,
            '.' => .period,
            '[' => .left_bracket,
            ']' => .right_bracket,
            ':' => .colon,
            '"' => {
                self.readChar();
                const start = self.position;
                _ = self.readString();
                defer self.readChar();
                return Token{ .type = .string, .start = start, .end = self.position };
            },
            0 => .eof,
            else => |c| if (isLetter(c)) {
                const start = self.position;
                const ident = self.readIdentifier();
                return Token{ .type = token.findType(ident), .start = start, .end = self.position };
            } else if (isDigit(c)) {
                const start = self.position;
                _ = self.readNumber();
                return Token{ .type = .integer, .start = start, .end = self.position };
            } else .illegal,
        };

        // read the next character so we don't read our last character
        // again when nexToken is called.
        defer self.readChar();

        return Token{
            .type = token_type,
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

    /// Reads a string from the current character
    fn readString(self: *Lexer) []const u8 {
        self.readChar(); // skip initial "
        const pos = self.position;
        while (self.char != '"' and self.char != 0) {
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
        \\"foo"
        \\"foo bar"
        \\"foo".len
        \\[1, 2]
        \\{"key":1}
    ;

    const tests = &[_]Token{
        .{ .type = .constant, .start = 0, .end = 5 },
        .{ .type = .identifier, .start = 6, .end = 10 },
        .{ .type = .assign, .start = 11, .end = 12 },
        .{ .type = .integer, .start = 13, .end = 14 },
        .{ .type = .constant, .start = 15, .end = 20 },
        .{ .type = .identifier, .start = 21, .end = 24 },
        .{ .type = .assign, .start = 25, .end = 26 },
        .{ .type = .integer, .start = 27, .end = 29 },
        .{ .type = .constant, .start = 30, .end = 35 },
        .{ .type = .identifier, .start = 36, .end = 39 },
        .{ .type = .assign, .start = 40, .end = 41 },
        .{ .type = .function, .start = 42, .end = 44 },
        .{ .type = .left_parenthesis, .start = 44, .end = 45 },
        .{ .type = .identifier, .start = 45, .end = 46 },
        .{ .type = .comma, .start = 46, .end = 47 },
        .{ .type = .identifier, .start = 48, .end = 49 },
        .{ .type = .right_parenthesis, .start = 49, .end = 50 },
        .{ .type = .left_brace, .start = 51, .end = 52 },
        .{ .type = .identifier, .start = 55, .end = 56 },
        .{ .type = .plus, .start = 57, .end = 58 },
        .{ .type = .identifier, .start = 59, .end = 60 },
        .{ .type = .right_brace, .start = 65, .end = 66 },
        .{ .type = .constant, .start = 68, .end = 73 },
        .{ .type = .identifier, .start = 74, .end = 80 },
        .{ .type = .assign, .start = 81, .end = 82 },
        .{ .type = .identifier, .start = 83, .end = 86 },
        .{ .type = .left_parenthesis, .start = 86, .end = 87 },
        .{ .type = .identifier, .start = 87, .end = 91 },
        .{ .type = .comma, .start = 91, .end = 92 },
        .{ .type = .identifier, .start = 93, .end = 96 },
        .{ .type = .right_parenthesis, .start = 96, .end = 97 },
        .{ .type = .bang, .start = 98, .end = 99 },
        .{ .type = .minus, .start = 99, .end = 100 },
        .{ .type = .slash, .start = 100, .end = 101 },
        .{ .type = .asterisk, .start = 101, .end = 102 },
        .{ .type = .integer, .start = 102, .end = 103 },
        .{ .type = .integer, .start = 104, .end = 105 },
        .{ .type = .less_than, .start = 106, .end = 107 },
        .{ .type = .integer, .start = 108, .end = 110 },
        .{ .type = .greater_than, .start = 111, .end = 112 },
        .{ .type = .integer, .start = 113, .end = 114 },
        .{ .type = ._if, .start = 116, .end = 118 },
        .{ .type = .left_parenthesis, .start = 119, .end = 120 },
        .{ .type = .integer, .start = 120, .end = 121 },
        .{ .type = .less_than, .start = 122, .end = 123 },
        .{ .type = .integer, .start = 124, .end = 126 },
        .{ .type = .right_parenthesis, .start = 126, .end = 127 },
        .{ .type = .left_brace, .start = 128, .end = 129 },
        .{ .type = ._return, .start = 132, .end = 138 },
        .{ .type = ._true, .start = 139, .end = 143 },
        .{ .type = .right_brace, .start = 144, .end = 145 },
        .{ .type = ._else, .start = 146, .end = 150 },
        .{ .type = .left_brace, .start = 151, .end = 152 },
        .{ .type = ._return, .start = 155, .end = 161 },
        .{ .type = ._false, .start = 162, .end = 167 },
        .{ .type = .right_brace, .start = 168, .end = 169 },
        .{ .type = .integer, .start = 171, .end = 173 },
        .{ .type = .equal, .start = 174, .end = 176 },
        .{ .type = .integer, .start = 177, .end = 179 },
        .{ .type = .integer, .start = 180, .end = 182 },
        .{ .type = .not_equal, .start = 183, .end = 185 },
        .{ .type = .integer, .start = 186, .end = 187 },
        .{ .type = .string, .start = 189, .end = 192 },
        .{ .type = .string, .start = 195, .end = 202 },
        .{ .type = .string, .start = 205, .end = 208 },
        .{ .type = .period, .start = 209, .end = 210 },
        .{ .type = .identifier, .start = 210, .end = 213 },
        .{ .type = .left_bracket, .start = 214, .end = 215 },
        .{ .type = .integer, .start = 215, .end = 216 },
        .{ .type = .comma, .start = 216, .end = 217 },
        .{ .type = .integer, .start = 218, .end = 219 },
        .{ .type = .right_bracket, .start = 219, .end = 220 },
        .{ .type = .left_brace, .start = 221, .end = 222 },
        .{ .type = .string, .start = 223, .end = 226 },
        .{ .type = .colon, .start = 227, .end = 228 },
        .{ .type = .integer, .start = 228, .end = 229 },
        .{ .type = .right_brace, .start = 229, .end = 230 },
        .{ .type = .eof, .start = 230, .end = 231 },
    };

    var lexer = Lexer.init(input);

    for (tests) |unit| {
        const current_token = lexer.next();

        testing.expectEqual(unit.start, current_token.start);
        testing.expectEqual(unit.end, current_token.end);
        testing.expectEqual(unit.type, current_token.type);
    }
}
