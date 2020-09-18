const std = @import("std");
const Value = @import("value.zig").OldValue;

pub const BuiltinError = error{ OutOfMemory, UnsupportedType, MismatchingTypes };
const BuiltinFn = Value.NativeFn;

pub const builtins = std.ComptimeStringMap(Value, .{
    .{ "len", len_func },
    .{ "add", add_func },
    .{ "pop", pop_func },
});

const len_func = Value{ .native = .{ .func = len, .arg_len = 0 } };
const add_func = Value{ .native = .{ .func = add, .arg_len = 1 } };
const pop_func = Value{ .native = .{ .func = pop, .arg_len = 0 } };

/// Returns the length of the `Value`.
/// Supports strings, arrays and maps.
fn len(allocator: *std.mem.Allocator, args: []*Value) BuiltinError!*Value {
    std.debug.assert(args.len == 1);
    const length: i64 = switch (args[0].*) {
        .string => |val| @intCast(i64, val.len),
        .list => |list| @intCast(i64, list.items.len),
        .map => |map| @intCast(i64, map.items().len),
        else => return BuiltinError.UnsupportedType,
    };
    return &Value{ .integer = length };
}

/// Appends a new value to the list
fn add(allocator: *std.mem.Allocator, args: []*Value) BuiltinError!*Value {
    std.debug.assert(args.len >= 2);
    return switch (args[0].*) {
        .list => |*list| {
            const val = args[args.len - 1];
            if (list.items.len > 0) {
                if (list.items[0].lufType() != val.lufType()) {
                    return BuiltinError.MismatchingTypes;
                }
            }
            try list.append(allocator, val);
            return args[0];
        },
        .map => |*map| {
            const key = args[args.len - 2];
            const val = args[args.len - 1];
            if (map.items().len > 0) {
                const entry = map.items()[0];
                if (entry.key.lufType() != key.lufType()) {
                    return BuiltinError.MismatchingTypes;
                }
                if (entry.value.lufType() != val.lufType()) {
                    return BuiltinError.MismatchingTypes;
                }
            }
            try map.put(allocator, key, val);
            return args[0];
        },
        else => BuiltinError.UnsupportedType,
    };
}

/// Pops the last value of a list and returns said value
fn pop(allocator: *std.mem.Allocator, args: []*Value) BuiltinError!*Value {
    std.debug.assert(args.len == 1);
    return switch (args[0].*) {
        .list => |*list| {
            return list.pop();
        },
        else => BuiltinError.UnsupportedType,
    };
}
