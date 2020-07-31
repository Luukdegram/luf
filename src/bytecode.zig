const std = @import("std");
const testing = std.testing;

/// Opcode for the virtual machine
pub const Opcode = enum(u8) {
    load_const,
    load_true,
    load_false,

    //bin ops
    add,
    sub,
    mul,
    div,

    // specifically removes a value from the stack
    pop,
};

/// Instruction contains the opcode and its data using native endian
pub const Instruction = struct {
    op: Opcode,
    ptr: u16,
};

/// Generates an instruction based on the opcode and an optional operand
/// We can load up to u16 size constants
pub fn gen(op: Opcode, operand: ?u16) Instruction {
    return .{
        .op = op,
        .ptr = operand orelse 0,
    };
}

/// List of instructions
pub const Instructions = std.ArrayList(Instruction);

test "generate instruction" {
    const test_cases = .{
        .{
            .op = Opcode.load_const,
            .operand = @as(u16, 65534),
            .expected = 65534,
        },
        .{
            .op = Opcode.add,
            .operand = null,
            .expected = 0,
        },
        .{
            .op = Opcode.load_false,
            .operand = null,
            .expected = 0,
        },
    };

    inline for (test_cases) |case| {
        const inst = gen(case.op, case.operand);
        testing.expect(case.op == inst.op);
        testing.expectEqual(@as(u16, case.expected), inst.ptr);
    }
}
