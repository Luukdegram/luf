const std = @import("std");
const testing = std.testing;

/// Opcode for the virtual machine
pub const Opcode = enum(u8) {
    // general ops
    load_const,
    load_true,
    load_false,
    load_nil,
    load_global,
    bind_global,
    load_local,
    bind_local,
    assign_global,
    assign_local,
    load_builtin,
    jump_false,
    jump,
    call,
    _return,
    return_value,
    load_module,

    make_array,
    make_map,

    //bin ops
    add,
    sub,
    mul,
    div,
    mod,
    equal,
    not_equal,
    greater_than,
    greater_than_equal,
    less_than,
    less_than_equal,
    minus,
    not,
    bitwise_or,
    bitwise_xor,
    bitwise_and,
    bitwise_not,
    shift_left,
    shift_right,
    @"and",
    @"or",
    assign_add,
    assign_sub,
    assign_mul,
    assign_div,

    // referencing
    get_by_index,
    set_by_index,

    /// specifically removes a value from the stack
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
