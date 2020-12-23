const Value = @import("../Value.zig");

/// Prints input to stdout
pub fn print(value: *Value) void {
    const writer = @import("std").io.getStdOut().writer();
    switch (value.ty) {
        .integer => writer.print("{d}", .{value.toInteger().value}) catch {},
        .string => writer.writeAll(value.toString().value) catch {},
        else => @import("std").debug.panic("TODO: Implement std.fmt.print for type {s}\n", .{@tagName(value.ty)}),
    }
}

/// Prints to stdout and appends a newline
pub fn println(fmt: []const u8) void {
    @import("std").io.getStdOut().writer().print("{}\n", .{fmt}) catch {};
}
