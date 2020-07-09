const std = @import("std");
const testing = std.testing;

/// Type Luf uses to define a token in the language
pub const Token = struct {
    type: TokenType,
    literal: []const u8,

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
