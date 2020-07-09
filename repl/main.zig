const std = @import("std");
const repl = @import("luf").repl;

pub fn main() !void {
    const out = std.io.getStdOut().writer();
    const in = std.io.getStdIn().reader();
    var allocator = std.heap.page_allocator;

    try repl.run(allocator, in, out);
}
