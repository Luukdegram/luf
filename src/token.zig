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
    pub const TokenType = enum {
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
        pipe,
        tilde,
        equal_add,
        equal_sub,
        equal_mul,
        equal_div,
        equal_ampersand,
        equal_caret,
        equal_pipe,
        shift_left,
        shift_right,
        double_period,
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
        for_loop,
        nil,
        import,
        // Reserved words in Zig, so we create literals
        @"and",
        @"or",
        @"true",
        @"false",
        @"if",
        @"else",
        @"return",
        @"continue",
        @"break",
        @"enum",
        @"switch",
        @"pub",
        // types
        bool_type,
        int_type,
        string_type,
        void_type,
    };

    /// Lookup 'table' to check if an identifier is a keyword
    pub const Keywords = std.ComptimeStringMap(TokenType, .{
        .{ "fn", .function },
        .{ "mut", .mutable },
        .{ "const", .constant },
        .{ "while", .while_loop },
        .{ "for", .for_loop },
        .{ "nil", .nil },
        .{ "import", .import },
        .{ "true", .@"true" },
        .{ "false", .@"false" },
        .{ "if", .@"if" },
        .{ "else", .@"else" },
        .{ "return", .@"return" },
        .{ "and", .@"and" },
        .{ "or", .@"or" },
        .{ "break", .@"break" },
        .{ "continue", .@"continue" },
        .{ "enum", .@"enum" },
        .{ "switch", .@"switch" },
        .{ "bool", .bool_type },
        .{ "int", .int_type },
        .{ "string", .string_type },
        .{ "void", .void_type },
        .{ "pub", .@"pub" },
    });

    /// Returns the string value of the token
    pub fn fmtString(token_type: comptime Token.TokenType) []const u8 {
        return switch (token_type) {
            .illegal => "[illegal]",
            .eof => "[eof]",
            // identifiers + literals
            .identifier => "[identifier]",
            .integer => "[integer]",
            .string => "[string]",
            .comment => "[comment]",
            .double_period => "..",
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
            .pipe => "|",
            .tilde => "~",
            .percent => "%",
            .period => ".",
            .equal_add => "+=",
            .equal_sub => "-=",
            .equal_mul => "*=",
            .equal_div => "/=",
            .equal_ampersand => "&=",
            .equal_caret => "^=",
            .equal_pipe => "|=",
            .shift_left => "<<",
            .shift_right => ">>",
            // delimiters
            .comma => ",",
            .left_parenthesis => "(",
            .right_parenthesis => ")",
            .left_brace => "{{", //escaped for fmt
            .right_brace => "}}", //escaped for fmt
            .left_bracket => "[",
            .right_bracket => "]",
            .colon => ":",
            // keywords
            .function => "fn",
            .mutable => "mut",
            .constant => "const",
            .while_loop => "while",
            .nil => "nil",
            .import => "import",
            .for_loop => "for",
            // literals because they are Zig keywords
            .@"and" => "and",
            .@"or" => "or",
            .@"true" => "true",
            .@"false" => "false",
            .@"if" => "if",
            .@"else" => "else",
            .@"return" => "return",
            .@"break" => "break",
            .@"continue" => "continue",
            .@"enum" => "enum",
            .@"switch" => "switch",
            .@"pub" => "pub",
            // types
            .bool_type => "bool",
            .string_type => "string",
            .int_type => "int",
            .void_type => "void",
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
        "or",
        "and",
        "return",
        "for",
        "continue",
        "break",
        "enum",
        "switch",
        "bool",
        "int",
        "string",
        "void",
        "pub",
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
