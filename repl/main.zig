const std = @import("std");
const repl = @import("repl.zig");

pub fn main() !void {
    const out = std.io.getStdOut().writer();
    const in = std.io.getStdIn().reader();
    var allocator = std.heap.page_allocator;

    try out.writeAll("---------======= Luf REPL =======---------\n");
    try repl.run(allocator, in, out);
}
