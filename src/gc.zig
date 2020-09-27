const std = @import("std");
const Allocator = std.mem.Allocator;
const Value = @import("value.zig").Value;
const Vm = @import("vm.zig").Vm;

const root = @import("root");

//! Mark-and-sweep based garbage collector
//! defining `gc_trigger_size` within the root file of your
//! program will allow to set the byte size of the new allocations before
//! the garbage collector is ran

pub const GarbageCollector = struct {
    /// The allocator used to allocate and free memory of the stack
    gpa: *Allocator,
    /// The stack that contains each allocated Value, used to track and then free its memory
    /// `stack` is a linked list to other Values
    stack: ?*Value,
    /// Reference to the Vm, used to retrieve the current stack and globals so we can
    /// mark the correct values before sweeping
    vm: *Vm,
    /// Size of currently allocated stack
    newly_allocated: usize,
    /// Amount of bytes allowed before the garbage collector is triggered for sweeping
    trigger_size: usize = if (@hasDecl(root, "gc_trigger_size")) root.gc_trigger_size else 512,

    /// Initializes a new `GarbageCollector`, calling deinit() will ensure all memory is freed
    pub fn init(allocator: *Allocator) GarbageCollector {
        return .{
            .gpa = allocator,
            .stack = null,
            .vm = undefined,
            .newly_allocated = 0,
        };
    }

    /// Inserts a new `Value` onto the stack and allocates `T` on the heap
    pub fn newValue(self: *GarbageCollector, comptime T: type, val: T) !*Value {
        const typed = try self.gpa.create(T);

        typed.* = val;

        typed.base = Value{
            .l_type = Value.Type.fromType(T),
            .next = self.stack,
            .is_marked = false,
        };

        self.stack = &typed.base;

        // In the future we could implement the Allocator interface so we get
        // direct access to the bytes being allocated, which will also allow tracking
        // of other allocations such as strings, lists, etc
        self.newly_allocated += @sizeOf(T);
        if (self.newly_allocated >= self.trigger_size) self.markAndSweep();

        return &typed.base;
    }

    /// Marks an object so it will not be sweeped by the gc.
    pub fn mark(self: *GarbageCollector, val: *Value) void {
        if (val.is_marked) return;
        std.debug.print("marked: {} {}\n", .{ @ptrToInt(val), val.l_type });

        val.is_marked = true;
        switch (val.l_type) {
            .iterable => self.mark(val.toIterable().value),
            .list => for (val.toList().value.items) |item| self.mark(item),
            .map => {
                for (val.toMap().value.items()) |entry| {
                    self.mark(entry.key);
                    self.mark(entry.value);
                }
            },
            else => {},
        }
    }

    /// Destroys all values not marked by the garbage collector
    pub fn sweep(self: *GarbageCollector) void {
        if (self.stack == null) return;

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
                std.debug.print("unmarking: {}\n", .{@ptrToInt(val)});
            }
        }
    }

    /// Finds all referenced values in the vm, marks them and finally sweeps unreferenced values
    fn markAndSweep(self: *GarbageCollector) void {
        for (self.vm.globals.items) |global| self.mark(global);
        for (self.vm.stack[0..self.vm.sp]) |stack| self.mark(stack);

        std.debug.print("Sweep at sp: {}\n", .{self.vm.sp});
        //self.sweep();
        self.newly_allocated = 0;
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
