pub const std = @import("std");
pub const byte_code = @import("../bytecode.zig");

pub fn codeGen(instructions: []byte_code.Instruction) void {
    for (instructions) |inst| gen(inst);
}

/// Generates WASM bytecode from Luf IR
fn gen(inst: byte_code.Instruction) void {
    switch (inst) {
        .op => {},
        .ptr => {},
        .integer => {},
        .string => {},
        .function => {},
    }
}
