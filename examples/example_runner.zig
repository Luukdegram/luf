const std = @import("std");
const luf = @import("luf");

const example = @import("build_options").example;

pub fn main() !void {
    if (example == null)
        return std.debug.print(
            "No example was provided, use -Dexample=example_name to run an example",
            .{},
        );

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = &gpa.allocator;

    const example_path = try std.fs.path.join(allocator, &[_][]const u8{
        "examples",
        example.?,
    });
    defer allocator.free(example_path);

    const example_file = try std.mem.concat(allocator, u8, &[_][]const u8{
        example_path,
        ".luf",
    });
    defer allocator.free(example_file);

    const file = std.fs.cwd().openFile(example_file, .{}) catch |_| {
        return std.debug.print("Example does not exist", .{});
    };
    defer file.close();

    const size = try file.getEndPos();

    const source = try file.readAllAlloc(allocator, size, size);
    defer allocator.free(source);

    const writer = std.io.getStdErr().writer();
    var vm = luf.Vm.init(allocator);
    defer vm.deinit();
    vm.compileAndRun(source) catch |err| {
        try vm.errors.write(source, writer);
        return err;
    };

    try vm.peek().print(writer);
    try writer.writeAll("\n");
}
