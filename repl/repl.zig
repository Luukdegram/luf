//! Implements a REPL, by lexing, parsing and then evaluating the input
//! it uses the reader and writer in the start function to read from and write to

const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("luf").Lexer;
const Token = @import("luf").Token;
const prompt = ">> ";

/// Runs the REPL, reads from the reader stream and outputs
/// to the writer stream.
pub fn run(allocator: *Allocator, reader: var, writer: var) !void {
    while (true) {
        // write our prompt
        try writer.writeAll(prompt);

        // read user input
        const input = try readLine(allocator, reader);

        // lex tokens
        var lexer = Lexer.init(input);
        var token = try allocator.create(Token);
        defer allocator.destroy(token);
        token.* = lexer.next();

        // print all tokens
        while (token.type != .eof) : (token.* = lexer.next()) {
            try writer.print("{}\n", .{token});
        }
    }
}

/// Reads a line from the reader
fn readLine(allocator: *Allocator, reader: var) ![]const u8 {
    var buffer = std.ArrayList(u8).init(allocator);
    var char: u8 = try reader.readByte();
    while (char != '\n') : (char = try reader.readByte()) {
        try buffer.append(char);
    }

    return buffer.toOwnedSlice();
}
