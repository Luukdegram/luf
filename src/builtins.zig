const std = @import("std");
const Value = @import("value.zig").Value;

pub const BuiltinError = error{ OutOfMemory, UnsupportedType, MismatchingTypes };
const BuiltinFn = Value.NativeFn;

pub const builtins = std.ComptimeStringMap(*Value, .{
    .{ "len", &len_func.base },
    .{ "add", &add_func.base },
    .{ "pop", &pop_func.base },
});

var len_func = Value.Native{ .base = .{ .l_type = .native, .is_marked = false, .next = null }, .func = len, .arg_len = 0 };
var add_func = Value.Native{ .base = .{ .l_type = .native, .is_marked = false, .next = null }, .func = add, .arg_len = 1 };
var pop_func = Value.Native{ .base = .{ .l_type = .native, .is_marked = false, .next = null }, .func = pop, .arg_len = 0 };

/// Returns the length of the `Value`.
/// Supports strings, arrays and maps.
fn len(allocator: *std.mem.Allocator, args: []*Value) BuiltinError!*Value {
    std.debug.assert(args.len == 1);
    const arg = args[0];
    const length: i64 = switch (arg.l_type) {
        .string => @intCast(i64, arg.toString().value.len),
        .list => @intCast(i64, arg.toList().value.items.len),
        .map => @intCast(i64, arg.toMap().value.items().len),
        else => return BuiltinError.UnsupportedType,
    };
    var integer = Value.Integer{ .value = length, .base = .{ .l_type = .integer, .next = null, .is_marked = false } };
    return &integer.base;
}

/// Appends a new value to the list
fn add(allocator: *std.mem.Allocator, args: []*Value) BuiltinError!*Value {
    std.debug.assert(args.len >= 2);
    return switch (args[0].l_type) {
        .list => {
            var list = args[0].toList().value;
            const val = args[args.len - 1];
            if (list.items.len > 0) {
                if (list.items[0].l_type != val.l_type) {
                    return BuiltinError.MismatchingTypes;
                }
            }
            try list.append(allocator, val);
            return args[0];
        },
        .map => {
            var map = args[0].toMap().value;
            const key = args[args.len - 2];
            const val = args[args.len - 1];
            if (map.items().len > 0) {
                const entry = map.items()[0];
                if (entry.key.l_type != key.l_type) {
                    return BuiltinError.MismatchingTypes;
                }
                if (entry.value.l_type != val.l_type) {
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
/// Assures the list as atleast 1 value
fn pop(allocator: *std.mem.Allocator, args: []*Value) BuiltinError!*Value {
    std.debug.assert(args.len == 1);
    return switch (args[0].l_type) {
        .list => {
            return args[0].toList().value.pop();
        },
        else => BuiltinError.UnsupportedType,
    };
}
