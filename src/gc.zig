const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;

pub const GarbageCollector = struct {
    /// The allocator used to allocate and free memory of the stack
    gpa: *Allocator,
    /// The stack that contains each allocated Value, used to track and then free its memory
    stack: std.AutoHashMap(Object.Handle, Object),
    /// Counter to create new indices for Objects
    object_index: u32,

    /// Initializes a new `GarbageCollector`, calling deinit() will ensure all memory is freed
    fn init(allocator: *Allocator) GarbageCollector {
        return .{
            .gpa = allocator,
            .stack = std.AutoHashMap(Object.Handle, Object),
            .object_index = 0,
        };
    }

    /// Inserts a new `Object` onto the stack and allocates the `Value` on the heap
    fn newObject(self: *GarbageCollector) *Value {
        const value = try self.gpa.create(Value);
        errdefer self.gpa.destroy(value);

        const handle = @intToEnum(Object.Handle, self.object_index);
        try self.stack.put(handle, .{
            .value = value,
            .handle = handle,
        });
        self.object_index += 1;
        return value;
    }
};

/// Wrapper struct around `Value` to track their usage and free
/// when possible by the gc
pub const Object = struct {
    /// The actual `Value` the object contains
    value: *Value,
    /// The unique id of the `Object`. Used by the gc to track it
    id: Handle,

    /// Unique id for each `Object`
    pub const Handle = enum(u32) { _ };
};
