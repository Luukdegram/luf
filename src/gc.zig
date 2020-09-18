const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").OldValue;

pub const GarbageCollector = struct {
    /// The allocator used to allocate and free memory of the stack
    gpa: *Allocator,
    /// The stack that contains each allocated Value, used to track and then free its memory
    stack: std.AutoArrayHashMap(Object.Handle, Object),
    /// Counter to create new indices for Objects
    object_index: u32,

    /// Initializes a new `GarbageCollector`, calling deinit() will ensure all memory is freed
    fn init(allocator: *Allocator) GarbageCollector {
        return .{
            .gpa = allocator,
            .stack = std.AutoArrayHashMap(Object.Handle, Object),
            .object_index = 0,
        };
    }

    /// Inserts a new `Object` onto the stack and allocates the `Value` on the heap
    fn newObject(self: *GarbageCollector) *Value {
        const value = try self.gpa.create(Value);
        errdefer self.gpa.destroy(value);

        const handle = @intToEnum(Object.Handle, self.object_index);
        const obj: Object = .{
            .value = value,
            .handle = handle,
        };
        try self.stack.put(handle, obj);
        self.object_index += 1;
        return obj;
    }

    /// Marks all objects on stack that are referenced
    pub fn mark(self: *GarbageCollector) void {
        for (self.stack.items()) |entry| {
            const object: Object = entry.value;
            switch(object.value.*) {

            }
        }
    }

    /// Marks an Object by its handle
    pub fn markObject(self: *GarbageCollector, handle: Object.Handle) void {
        if (self.stack.get(handle)) |object| object.reference_count += 1;
    }
};

/// Wrapper struct around `Value` to track their usage and free
/// when possible by the gc
pub const Object = struct {
    /// The actual `Value` the object contains
    value: *Value,
    /// The unique id of the `Object`. Used by the gc to track it
    id: Handle,
    /// Counter for each refernce it holds
    reference_count: u32,

    /// Unique id for each `Object`
    pub const Handle = enum(u32) { _ };
};
