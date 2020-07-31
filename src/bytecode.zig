const std = @import("std");
const testing = std.testing;

/// Opcode for the virtual machine
pub const Opcode = enum(u8) {
    load_const,
    add,
};

/// Instruction contains the opcode and its data using native endian
pub const Instruction = struct {
    op: Opcode,
    data: []const u8,
};

/// Generates an instruction based on the opcode and an optional operand
/// We can load up to u16 size constants
pub fn gen(op: Opcode, operand: ?u16) Instruction {
    return .{
        .op = op,
        .data = if (operand) |oper| &std.mem.toBytes(oper) else "",
    };
}

/// List of instructions
pub const Instructions = std.ArrayList(Instruction);

test "generate instruction" {
    const test_cases = .{
        .{
            .op = Opcode.load_const,
            .operand = @as(u16, 65534),
            .expected = &[_]u8{ 254, 255 },
        },
        .{
            .op = Opcode.add,
            .operand = null,
            .expected = &[_]u8{},
        },
    };

    inline for (test_cases) |case| {
        const inst = gen(case.op, case.operand);
        testing.expect(case.op == inst.op);
        testing.expectEqualSlices(u8, case.expected, inst.data);
    }
}
