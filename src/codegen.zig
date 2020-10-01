pub const std = @import("std");
pub const byte_code = @import("bytecode.zig");
pub const wasm = @import("codegen/wasm.zig");

pub const Arch = enum {
    wasm,
};

/// Emits bytecode dependent on given `arch`. i.e. emits WASM bytecode when arch = .wasm
pub fn gen(arch: Arch, instructions: []byte_code.Instruction) void {
    switch (arch) {
        .wasm => wasm.codeGen(instructions),
    }
}
