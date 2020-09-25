/// Prints input to stdout
pub fn print(fmt: []const u8) void {
    @import("std").io.getStdOut().writer().writeAll(fmt) catch {};
}

/// Prints to stdout and appends a newline
pub fn println(fmt: []const u8) void {
    @import("std").io.getStdOut().writer().print("{}\n", .{fmt}) catch {};
}
