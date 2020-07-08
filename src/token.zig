const std = @import("std");

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
    };

    /// Lookup 'table' to check if an identifier is a keyword
    pub const Keywords = std.ComptimeStringMap(TokenType, .{
        .{ "fn", .function },
        .{ "mut", .mutable },
        .{ "const", .constant },
    });

    pub fn init(token_type: Token.TokenType, char: u8) Token {
        return Token{ .type = token_type, .literal = &[_]u8{char} };
    }
};

/// Returns the correct type of the identifier.
/// First checks if it's a keyword and returns the corresponding keyword,
/// if no keyword is found, returns `.identifier`.
pub fn findType(identifier: []const u8) Token.TokenType {
    return Token.Keywords.get(identifier) orelse .identifier;
}
