const std = @import("std");
const testing = std.testing;

/// Type Luf uses to define a token in the language
pub const Token = struct {
    type: TokenType,
    start: usize,
    end: usize,

    /// Identifiers that are considered a token
    pub const TokenType = union(enum) {
        illegal,
        eof,
        // identifiers + literals
        identifier,
        integer,
        // operators
        assign,
        plus,
        minus,
        bang,
        asterisk,
        slash,
        less_than,
        greater_than,
        equal,
        not_equal,
        // delimiters
        comma,
        left_parenthesis,
        right_parenthesis,
        left_brace,
        right_brace,
        // keywords
        function,
        mutable,
        constant,
        // underscores because reserved words in Zig
        _true,
        _false,
        _if,
        _else,
        _return,
    };

    /// Lookup 'table' to check if an identifier is a keyword
    pub const Keywords = std.ComptimeStringMap(TokenType, .{
        .{ "fn", .function },
        .{ "mut", .mutable },
        .{ "const", .constant },
        .{ "true", ._true },
        .{ "false", ._false },
        .{ "if", ._if },
        .{ "else", ._else },
        .{ "return", ._return },
    });

    /// Returns the string value of the token
    pub fn string(self: Token) []const u8 {
        return switch (self.type) {
            .illegal => "[illegal]",
            .eof => "[eof]",
            // identifiers + literals
            .identifier => "[identifier]",
            .integer => "[integer]",
            // operators
            .assign => "=",
            .plus => "+",
            .minus => "-",
            .bang => "!",
            .asterisk => "*",
            .slash => "/",
            .less_than => "<",
            .greater_than => ">",
            .equal => "==",
            .not_equal => "!=",
            // delimiters
            .comma => ",",
            .left_parenthesis => "(",
            .right_parenthesis => ")",
            .left_brace => "{",
            .right_brace => "}",
            // keywords
            .function => "fn",
            .mutable => "mut",
            .constant => "const",
            // underscores because reserved words in Zig
            ._true => "true",
            ._false => "false",
            ._if => "if",
            ._else => "else",
            ._return => "return",
        };
    }
};

/// Returns the correct type of the identifier.
/// First checks if it's a keyword and returns the corresponding keyword,
/// if no keyword is found, returns `.identifier`.
pub fn findType(identifier: []const u8) Token.TokenType {
    return Token.Keywords.get(identifier) orelse .identifier;
}

test "Keywords" {
    const keywords = &[_][]const u8{
        "fn",
        "mut",
        "const",
        "true",
        "if",
        "else",
        "return",
    };

    for (keywords) |keyword| {
        testing.expect(findType(keyword) != .identifier);
    }
}

test "Identifiers" {
    const identifiers = &[_][]const u8{
        "a",
        "word",
        "random",
    };
    for (identifiers) |identifier| {
        testing.expect(findType(identifier) == .identifier);
    }
}
