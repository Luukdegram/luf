const std = @import("std");
const testing = std.testing;

//! Contains what Luf sees as tokens
//! This file solely contains the definitions of all tokens
//! the parsing of the tokens can be found in the `Lexer`.

/// Type Luf uses to define a token in the language
pub const Token = struct {
    token_type: TokenType,
    start: usize,
    end: usize,

    /// Identifiers that are considered a token
    pub const TokenType = union(enum) {
        illegal,
        eof,
        period,
        // identifiers + literals
        identifier,
        integer,
        string,
        comment,
        // operators
        assign,
        plus,
        minus,
        percent,
        bang,
        asterisk,
        slash,
        less_than,
        greater_than,
        less_than_equal,
        greater_than_equal,
        equal,
        not_equal,
        ampersand,
        caret,
        vertical_line,
        equal_add,
        equal_sub,
        equal_mul,
        equal_div,
        equal_ampersand,
        equal_caret,
        equal_vertical_line,
        shift_left,
        shift_right,
        // delimiters
        comma,
        left_parenthesis,
        right_parenthesis,
        left_brace,
        right_brace,
        left_bracket,
        right_bracket,
        colon,
        // keywords
        function,
        mutable,
        constant,
        while_loop,
        nil,
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
        .{ "while", .while_loop },
        .{ "nil", .nil },
        .{ "true", ._true },
        .{ "false", ._false },
        .{ "if", ._if },
        .{ "else", ._else },
        .{ "return", ._return },
    });

    /// Returns the string value of the token
    pub fn string(token_type: comptime Token.TokenType) []const u8 {
        return switch (token_type) {
            .illegal => "[illegal]",
            .eof => "[eof]",
            // identifiers + literals
            .identifier => "[identifier]",
            .integer => "[integer]",
            .string => "[string]",
            .comment => "[comment]",
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
            .less_than_equal => "<=",
            .greater_than_equal => ">=",
            .ampersand => "&",
            .caret => "^",
            .vertical_line => "|",
            .equal_add => "+=",
            .equal_sub => "-=",
            .equal_mul => "*=",
            .equal_div => "/=",
            .equal_ampersand => "&=",
            .equal_caret => "^=",
            .equal_vertical_line => "|=",
            .shift_left => "<<",
            .shift_right => ">>",
            // delimiters
            .comma => ",",
            .left_parenthesis => "(",
            .right_parenthesis => ")",
            .left_brace => "{",
            .right_brace => "}",
            .left_bracket => "[",
            .right_bracket => "]",
            .colon => ":",
            // keywords
            .function => "fn",
            .mutable => "mut",
            .constant => "const",
            .while_loop => "while",
            .nil => "nil",
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
        "false",
        "if",
        "else",
        "return",
        "while",
        "nil",
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
