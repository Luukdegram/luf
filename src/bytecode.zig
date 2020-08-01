const std = @import("std");
const testing = std.testing;

/// Opcode for the virtual machine
pub const Opcode = enum(u8) {
    load_const,
    load_true,
    load_false,
    load_nil,
    jump_false,
    jump,

    //bin ops
    add,
    sub,
    mul,
    div,
    equal,
    not_equal,
    greater_than,
    less_than,
    minus,
    bang,

    // specifically removes a value from the stack
    pop,
};

/// Instruction contains the opcode and its data using native endian
pub const Instruction = struct {
    /// Opcode for the instruction
    op: Opcode,
    /// Points to index of constant value
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
