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

        typed.base = Value{
            .l_type = Value.Type.fromType(T),
            .next = self.stack,
            .is_marked = false,
        };

        self.stack = &typed.base;

        return &typed.base;
    }

    /// Marks an object so it will not be sweeped by the gc.
    pub fn mark(self: *GarbageCollector, val: *Value) void {
        val.is_marked = true;
        switch (val.l_type) {
            .iterable => val.unwrap(.iterable).?.value.is_marked = true,
            .list => {
                for (val.toList().value.items) |item|
                    item.is_marked = true;
            },
            .map => {
                const map = val.toMap().value;
                for (map.items()) |entry| {
                    entry.key.is_marked = true;
                    entry.value.is_marked = true;
                }
            },
            else => {},
        }
    }

    /// Destroys all values not marked by the garbage collector
    pub fn sweep(self: *GarbageCollector, values: []const *Value) void {
        if (self.stack == null) return;

        for (values) |val| self.mark(val);

        var prev: ?*Value = null;
        var next: ?*Value = self.stack;
        while (next) |val| {
            next = val.next;
            if (!val.is_marked) {
                if (prev) |p| p.next = next else self.stack = next;
                val.destroy(self.gpa);
            } else {
                val.is_marked = false;
                prev = val;
            }
        }
    }

    /// Frees the `stack` that still exist on exit
    pub fn deinit(self: *GarbageCollector) void {
        while (self.stack) |next| {
            const temp = next.next;
            next.destroy(self.gpa);
            self.stack = temp;
        }
        self.* = undefined;
    }
};
