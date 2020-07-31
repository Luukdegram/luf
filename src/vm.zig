const std = @import("std");
const compiler = @import("compiler.zig");
const ByteCode = compiler.Compiler.ByteCode;
const Value = @import("value.zig").Value;
const testing = std.testing;

//! The Virtual Machine of Luf is stack-based.
//! Currently the stack has a size of 2048 (pre-allocated)

pub fn run(byte_code: ByteCode) Vm.Error!Vm {
    var vm = Vm{};

    // instruction pointer
    var ip: usize = 0;
    for (byte_code.instructions) |inst| {
        switch (inst.op) {
            .load_const => {
                const index = std.mem.bytesToValue(u16, inst.data[0..2]);
                try vm.push(byte_code.constants[index]);
            },
            .add => {
                const left = vm.pop().?;
                const right = vm.pop().?;

                const result = left.integer + right.integer;
                try vm.push(.{ .integer = result });
            },
            else => std.debug.panic("TODO Implement operator: {}", .{inst.op}),
        }
    }

    return vm;
}

/// VM is a stack-based Virtual Machine
/// Although a register-based VM is more performant,
/// a stack-based one is more understandable
pub const Vm = struct {
    /// Stack pointer holds the current stack position
    sp: usize = 0,
    /// Stack has a max of 2048 Value's that it can hold
    /// This is pre-allocated.
    stack: [2048]Value = undefined,

    pub const Error = error{OutOfMemory};

    /// Pushes a new `Value` on top of the `stack` and increases
    /// the stack pointer by 1.
    fn push(self: *Vm, value: Value) Error!void {
        if (self.sp > self.stack.len - 1) return Error.OutOfMemory;

        self.stack[self.sp] = value;
        self.sp += 1;
    }

    /// Returns the `Value` from the `stack` and decreases the stack
    /// pointer by 1. Returns null if stack is empty.
    fn pop(self: *Vm) ?Value {
        if (self.sp == 0) return null;
        defer self.sp -= 1;
        return self.stack[self.sp - 1];
    }
};

test "Integer arithmetic" {
    const test_cases = .{
        .{ .input = "1", .expected = 1 },
        .{ .input = "2", .expected = 2 },
        .{ .input = "1 + 1", .expected = 2 },
    };

    inline for (test_cases) |case| {
        const byte_code = try compiler.compile(testing.allocator, case.input);
        defer byte_code.deinit();
        var vm = try run(byte_code);
        testing.expectEqual(@as(i64, case.expected), vm.pop().?.integer);
    }
}
