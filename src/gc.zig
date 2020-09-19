const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;

pub const GarbageCollector = struct {
    /// The allocator used to allocate and free memory of the stack
    gpa: *Allocator,
    /// The stack that contains each allocated Value, used to track and then free its memory
    /// `stack` is a linked list to other Values
    stack: ?*Value,

    /// Initializes a new `GarbageCollector`, calling deinit() will ensure all memory is freed
    pub fn init(allocator: *Allocator) GarbageCollector {
        return .{
            .gpa = allocator,
            .stack = null,
        };
    }

    /// Inserts a new `Value` onto the stack and allocates `T` on the heap
    pub fn newValue(self: *GarbageCollector, comptime T: type) !*Value {
        const typed = try self.gpa.create(T);
        errdefer self.gpa.destroy(typed);

        typed.base = Value{
            .l_type = Value.Type.fromType(T),
            .next = self.stack,
            .is_marked = false,
        };

        self.stack = &typed.base;

        return &typed.base;
    }

    /// Marks all objects on stack that are referenced
    pub fn mark(self: *GarbageCollector, values: []*Value) void {
        for (values) |val| val.is_marked = true;
    }

    /// Destroys all values not marked by the garbage collector
    pub fn sweep(self: *GarbageCollector) void {
        if (self.stack == null) return;

        var prev: *Value = undefined;
        while (self.stack.?.next) |*val| {
            if (!val.is_marked) {
                if (val.next) |next| prev.next = next;
                val.destroy(self.gpa);
                continue;
            }
            prev = val;
        }
    }

    /// Frees the `stack` that still exist on exit
    pub fn deinit(self: *GarbageCollector) void {
        if (self.stack) |stack| {
            defer stack.destroy(self.gpa);
            while (stack.next) |next| next.destroy(self.gpa);
        }
        self.* = undefined;
    }
};
