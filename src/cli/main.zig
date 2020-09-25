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

    if (std.mem.eql(u8, action, "run")) {
        try runner(gpa, file_path);
        return;
    }

    var i: usize = 0;
    const output_name = while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "-o") and args.len > i + 1) {
            break args[i + 1];
        }
    } else null;

    try builder(gpa, file_path, output_name);
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

/// Runs either a Luf source file or bytecode directly
fn runner(gpa: *std.mem.Allocator, file_path: []const u8) !void {
    const file_type: enum { source, byte_code } = if (std.mem.endsWith(u8, file_path, ".luf"))
        .source
    else if (std.mem.endsWith(u8, file_path, ".blf"))
        .byte_code
    else
        return log.err("Unsupported file type, expected a '.luf' or '.blf' file\n", .{});

    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        return log.err("Could not open file: '{}'\nError: {}\n", .{ file_path, @errorName(err) });
    };

    var vm = try luf.Vm.init(gpa);
    defer vm.deinit();

    if (file_type == .source) {
        const file_data = try file.readToEndAlloc(gpa, std.math.maxInt(u64));
        defer gpa.free(file_data);

        vm.compileAndRun(file_data) catch |err| {
            try vm.errors.write(file_data, std.io.getStdErr().writer());
        };
        return;
    }

    var byte_code = luf.byte_code.ByteCode.decodeFromStream(gpa, file.reader()) catch |err| {
        log.err("Could not decode bytecode: {}\n", .{@errorName(err)});
        return err;
    };
    defer byte_code.deinit();

    vm.loadCode(&byte_code);
    vm.run() catch {
        try vm.errors.write("", std.io.getStdErr().writer());
    };
}

/// Compiles the luf source code and outputs its bytecode which can then be ran later
fn builder(gpa: *std.mem.Allocator, file_path: []const u8, output_name: ?[]const u8) !void {
    if (!std.mem.endsWith(u8, file_path, ".luf"))
        return log.err("Expected file with luf extension: '.luf' in path '{}'\n", .{file_path});

    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        return log.err("Could not open file with path '{}'\nError recieved: {}\n", .{ file_path, err });
    };

    const source = try file.readToEndAlloc(gpa, std.math.maxInt(u64));
    defer gpa.free(source);

    var errors = luf.Errors.init(gpa);
    defer errors.deinit();

    var byte_code = luf.compiler.compile(gpa, source, &errors) catch |err| {
        return errors.write(source, std.io.getStdErr().writer());
    };
    defer byte_code.deinit();

    const final_output_name = output_name orelse blk: {
        const base = std.fs.path.basename(file_path);
        var new_name = try gpa.alloc(u8, base.len);
        std.mem.copy(u8, new_name, base);
        std.mem.copy(u8, new_name[new_name.len - 3 .. new_name.len], "blf");
        break :blk new_name;
    };
    // if `output_name` was null, we allocated memory to create the new name
    defer if (output_name == null) gpa.free(final_output_name);

    const output_file = std.fs.cwd().createFile(final_output_name, .{}) catch |err| {
        return log.err("Could not create output file '{}'\nError '{}'\n", .{ final_output_name, err });
    };

    byte_code.encodeToStream(output_file.writer()) catch |err| {
        return log.err("Could not write to output file '{}'\nError '{}'\n", .{ final_output_name, err });
    };
}
