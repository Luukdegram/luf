pub const std = @import("std");
pub const byte_code = @import("../bytecode.zig");

/// Generates WASM IR which can then be emitted to bytecode
pub fn codeGen(instructions: []byte_code.Instruction) void {
    for (instructions) |inst| gen(inst);
}

const ops = enum(u8) {
    @"unreachable" = 0x00,
    nop = 0x01,
    block = 0x02,
    loop = 0x03,
    @"if" = 0x04,
    @"else" = 0x05,
    @"break" = 0x0C,
    break_if = 0x0D,
    break_table = 0x0E,
    @"return" = 0x0F,
    call = 0x10,
    call_ind = 0x11,
    drop = 0x1A,
    select = 0x1B,
    locat_get = 0x20,
    local_set = 0x21,
    local_tee = 0x22,
    global_get = 0x23,
    global_set = 0x24,
    // luf uses i64's so for now support just that
    i64_load = 0x29,
    // same as above, just i64 for now
    i64_store = 0x37,
    mem_size = 0x3F,
    mem_grow = 0x40,
    i64_const = 0x41,
    eqz = 0x45,
    eq = 0x46,
    ne = 0x47,
    lt_s = 0x48,
    lt_u = 0x49,
};

const Wasm = union {
    op: ops,
    ptr: struct { op: ops, id: u8 },
    i64: i64,
};

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

comptime {
    std.meta.refAllDecls(@This());
}
