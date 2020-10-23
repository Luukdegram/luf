pub const std = @import("std");
pub const lir = @import("ir.zig");
const leb = std.debug.leb;

const Op = enum(u8) {
    @"unreachable" = 0x00,
    nop = 0x01,
    block = 0x02,
    loop = 0x03,
    @"if" = 0x04,
    @"else" = 0x05,
    end = 0x0B,
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
    i64_const = 0x42,
    eqz = 0x45,
    eq = 0x46,
    ne = 0x47,
    lt_s = 0x48,
    lt_u = 0x49,
};

/// Utility struct with helper functions to make it easier
/// to manage instructions
pub const Instructions = struct {
    buffer: std.ArrayList(u8),

    pub const Error = error{OutOfMemory};

    /// Creates a new instance of `Instructions`
    pub fn init(gpa: *std.mem.Allocator) Instructions {
        return .{ .buffer = std.ArrayList(u8).init(gpa) };
    }

    /// Creates a new instance of `Instructions` aswell as codegen bytecode instructions from Luf's IR
    pub fn fromCu(gpa: *std.mem.Allocator, cu: lir.CompileUnit) Error![]const u8 {
        var instructions = init(gpa);

        // First 5 bytes contain the size of the generated code
        instructions.buffer.resize(5);

        for (cu.instructions) |inst| {
            try instructions.gen(inst);
        }

        try instructions.emit(.end);
        return instructions.final();
    }

    /// Returns the current instruction set as Wasm
    /// This invalidates the current `list` of instructions
    /// It is not required to call deinit() after calling this
    pub fn final(self: *Instructions) []const u8 {
        return self.buffer.toOwnedSlice();
    }

    /// Frees the `list` of `Instructions`. This function is not required
    /// if final() has been called
    pub fn deinit(self: *Instructions) void {
        self.buffer.deinit(self.gpa);
    }

    /// Generates and appends instructions based on the given IR instruction
    fn gen(self: *Instructions, inst: *lir.Inst) Error!void {
        switch (inst.tag) {
            .add,
            .sub,
            .mul,
            .div,
            .eql,
            .nql,
            .lt,
            .gt,
            .assign_add,
            .assign_sub,
            .assign_mul,
            .assign_div,
            .bitwise_xor,
            .bitwise_or,
            .bitwise_and,
            .shift_left,
            .shift_right,
            .@"and",
            .@"or",
            .eql_lt,
            .eql_gt,
            .mod,
            => try self.emitInfix(inst.as(lir.Inst.Double)),
            .not, .bitwise_not, .negate => try self.emitPrefix(inst.as(lir.Inst.Single)),
            .int => try self.emitInt(inst.as(lir.Inst.Int)),
            .string => try self.emitString(inst.as(lir.Inst.String)),
            .primitive => try self.emitPrim(inst.as(lir.Inst.Primitive)),
            .ident => try self.emitIdent(inst.as(lir.Inst.Ident)),
            .expr => try self.emitExpr(inst.as(lir.Inst.Single)),
            .decl => try self.emitDecl(inst.as(lir.Inst.Decl)),
            .@"return" => try self.emitRet(inst.as(lir.Inst.Single)),
            .assign => try self.emitAssign(inst.as(lir.Inst.Double)),
            .store => try self.emitStore(inst.as(lir.Inst.Triple)),
            .load => try self.emitLoad(inst.as(lir.Inst.Double)),
            .list, .map => try self.emitList(inst.as(lir.Inst.DataStructure)),
            .pair => try self.emitPair(inst.as(lir.Inst.Double)),
            .range => try self.emitRange(inst.as(lir.Inst.Double)),
            .import => try self.emitModule(inst.as(lir.Inst.String)),
            .@"enum" => try self.emitEnum(inst.as(lir.Inst.Enum)),
            .condition => try self.emitCond(inst.as(lir.Inst.Condition)),
            .block => try self.emitBlock(inst.as(lir.Inst.Block)),
            .func => try self.emitFunc("", inst.as(lir.Inst.Function)),
            .call => try self.emitCall(inst.as(lir.Inst.Call)),
            .@"while" => try self.emitWhile(inst.as(lir.Inst.Double)),
            .@"switch" => try self.emitSwitch(inst.as(lir.Inst.Switch)),
            .branch => try self.emitBranch(inst.as(lir.Inst.Double)),
            .@"break" => try self.scope.loop.jumps.append(self.gpa, try self.label(.jump)),
            .@"continue" => try self.emitPtr(.jump, self.scope.loop.start),
            .@"for" => try self.emitLoop(inst.as(lir.Inst.Loop)),
            .comment, .type_def => {}, //VM doesn't do anything with this
        }
    }

    /// Writes an unsigned integer value
    fn emitUnsigned(self: *Instructions, value: anytype) !void {
        try leb.writeULEB128(self.buffer.writer(), value);
    }

    /// Writes a signed integer value
    fn emitSigned(self: *Instructions, value: anytype) !void {
        try leb.writeILEB128(self.buffer.writer(), value);
    }

    /// Emits an integer
    fn emitInt(self: *Instructions, int: *lir.Inst.Int) !void {
        try self.emit(.i64_const);
        try self.emitSigned(int.value);
    }

    /// Emits a single opcode
    fn emit(self: *Instructions, op: Op) !void {
        try self.buffer.writer().writeByte(@enumToInt(op));
    }

    /// Emits a Wasm block instruction
    fn emitBlock(self: *Instructions, block: *lir.Inst.Block) !void {
        try self.emit(.block);
        for (block.instructions) |inst| try self.gen(inst);
        try self.emit(.end);
    }
};

comptime {
    std.meta.refAllDecls(@This());
}
