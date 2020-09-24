const std = @import("std");
const luf = @import("luf");

const log = std.log.scoped(.luf_cli);

pub fn main() !void {
    var gpa_alloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_alloc.deinit();
    const gpa = &gpa_alloc.allocator;

    const args = try parse(gpa);
    defer {
        for (args) |arg| gpa.free(arg);
        gpa.free(args);
    }

    const action = if (args.len >= 2) args[1] else return log.err("Missing argument 'build' or 'run'\n", .{});

    if (!std.mem.eql(u8, action, "build") and !std.mem.eql(u8, action, "run")) return log.err("Invalid command '{}'\n", .{action});

    const file_path = if (args.len >= 3) args[2] else return log.err("Missing file path: 'luf {} <file_path>'\n", .{action});

    const file_type: enum { source, byte_code } = if (std.mem.endsWith(u8, file_path, ".luf"))
        .source
    else if (std.mem.endsWith(u8, file_path, ".blf"))
        .byte_code
    else
        return log.err("Unsupported file type, expected a '.luf' or '.blf' file\n", .{});

    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        return log.err("Could not open file: '{}'\nError: {}\n", .{ file_path, err });
    };

    const file_data = try file.readToEndAlloc(gpa, std.math.maxInt(u64));
    defer gpa.free(file_data);

    var vm = try luf.Vm.init(gpa);
    defer vm.deinit();

    if (file_type == .source) {
        vm.compileAndRun(file_data) catch |err| {
            try vm.errors.write(file_data, std.io.getStdErr().writer());
            return;
        };
    }
}

/// Parses the process' arguments
fn parse(gpa: *std.mem.Allocator) ![][]const u8 {
    var list = std.ArrayList([]const u8).init(gpa);
    errdefer list.deinit();
    var args = std.process.args();

    while (args.next(gpa)) |arg| {
        try list.append(try arg);
    }

    return list.toOwnedSlice();
}
