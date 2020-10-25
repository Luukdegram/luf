pub const std = @import("std");
pub const lir = @import("ir.zig");
pub const LufType = @import("value.zig").Value.Type;
const Allocator = std.mem.Allocator;
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
    call_indirect = 0x11,
    drop = 0x1A,
    select = 0x1B,
    local_get = 0x20,
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

/// Section id's as described at:
/// https://webassembly.github.io/spec/core/binary/modules.html#sections
const SectionType = enum {
    custom,
    @"type",
    import, // not used in Luf, everything is compiled to 1 compile unit
    func,
    table,
    memory,
    global,
    @"export",
    start,
    element,
    code,
    data,
};

const Section = struct {
    ty: SectionType,
    code: std.ArrayList(u8),
    size: u32,

    fn init(ty: SectionType, gpa: *Allocator) Section {
        return .{ .ty = ty, .code = std.ArrayList(u8).init(gpa), .size = 0 };
    }

    /// Emits all code from the section into the writer
    /// This invalidates the code saved into section itself
    fn emit(self: Section, writer: anytype) !void {
        if (self.code.items.len == 0) return;

        // section id
        writer.writeByte(@enumToInt(self.ty));

        // count
        leb.writeULEB128(writer, self.size);

        writer.writeAll(self.code.toOwnedSlice(gpa));
    }
};

/// Contains all possible types as described at
/// https://webassembly.github.io/spec/core/binary/types.html
const Types = struct {
    const block: u8 = 0x40;
    const func: u8 = 0x60;
    const table: u8 = 0x70;

    /// Limits as described at:
    /// https://webassembly.github.io/spec/core/binary/types.html#limits
    const Limits = enum(u1) {
        zero,
        one,
    };

    /// Wasm Value types as described at:
    /// https://webassembly.github.io/spec/core/binary/types.html#value-types
    const Value = enum(u7) {
        I32 = 0x7F,
        I64 = 0x7E,
        F32 = 0x7D,
        F64 = 0x7C,
    };

    /// Export sections as described at:
    /// http://webassembly.github.io/spec/core/binary/modules.html#export-section
    const Export = enum {
        func = 0x00,
        table = 0x01,
        mem = 0x02,
        global = 0x03,
    };
};

// Magic constants, required for a valid Wasm module
const module_header = [_]u8{ 0x00, 0x61, 0x73, 0x6D }; // \x00asm
const module_version = [_]u8{ 0x01, 0x00, 0x00, 0x00 }; // v1

/// Utility struct with helper functions to make it easier
/// to manage instructions
pub const Wasm = struct {
    buffer: std.ArrayList(u8),
    sections: std.ArrayListUnmanaged(Section),
    gpa: *Allocator,

    pub const Error = error{OutOfMemory};

    /// Creates a new instance of `Instructions`
    pub fn init(gpa: *Allocator) Wasm {
        return .{
            .buffer = std.ArrayList(u8).init(gpa),
            .sections = std.ArrayListUnmanaged(Section),
            .gpa = gpa,
        };
    }

    /// Creates a new instance of `Instructions` aswell as codegen bytecode instructions from Luf's IR
    pub fn fromCu(gpa: *Allocator, cu: lir.CompileUnit) Error![]const u8 {
        var wasm = init(gpa);
        wasm.writer().writeIntLittle(u32, module_header);
        wasm.writer().writeIntLittle(u32, module_version);

        for (cu.instructions) |inst| {
            try wasm.gen(inst);
        }

        return wasm.final();
    }

    /// Returns the current instruction set as Wasm
    /// This invalidates the current `buffer` of instructions
    /// It is not required to call deinit() after calling this
    pub fn final(self: *Wasm) []const u8 {
        return self.buffer.toOwnedSlice();
    }

    /// Frees the `list` of `Instructions`. This function is not required
    /// if final() has been called
    pub fn deinit(self: *Wasm) void {
        self.buffer.deinit(self.gpa);
    }

    /// Wrapper around internal `buffer`'s writer(), allows for code completion
    /// and easier access to its writer
    fn writer(self: *Wasm) std.io.Writer(Wasm, Error, self.buffer.writer) {
        return self.buffer.writer;
    }

    /// Resolves the Wasm's value type given the LufType
    fn resolveValType(ty: LufType) Types.Value {
        switch (ty) {
            .integer => .I64,
            else => @panic("TODO: Implement more types for wasm"),
        }
    }

    /// Generates and appends instructions based on the given IR instruction
    fn gen(self: *Wasm, inst: *lir.Inst) Error!void {
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
    fn emitUnsigned(self: *Wasm, value: anytype) !void {
        try leb.writeULEB128(self.buffer.writer(), value);
    }

    /// Writes a signed integer value
    fn emitSigned(self: *Wasm, value: anytype) !void {
        try leb.writeILEB128(self.buffer.writer(), value);
    }

    /// Emits an integer
    fn emitInt(self: *Wasm, int: *lir.Inst.Int) !void {
        try self.emit(.i64_const);
        try self.emitSigned(int.value);
    }

    /// Emits a single opcode
    fn emit(self: *Wasm, op: Op) !void {
        try self.buffer.writer().writeByte(@enumToInt(op));
    }

    /// Emits a raw byte
    fn raw(self: *Wasm, c: u8) !void {
        try self.buffer.writer().writeByte(c);
    }

    /// Emits a Wasm block instruction
    fn emitBlock(self: *Wasm, block: *lir.Inst.Block) !void {
        for (block.instructions) |inst| try self.gen(inst);
    }

    /// Loads a local or global variable onto the stack
    fn emitIdent(self: *Wasm, ident: *lir.Inst.Ident) !void {
        try self.emit(if (ident.scope == .global) .global_get else .local_get);
        try self.emitUnsigned(ident.index);
    }

    /// Emits a return statement in Wasm
    fn emitRet(self: *Wasm, ret: *lir.Inst.Single) !void {
        try self.gen(ret.rhs);
        try self.emit(.@"return");
    }

    /// Emits wasm for the declation's value to put it on the stack
    /// and then generates a .local_set or .global_set based on its scope
    fn emitDecl(self: *Wasm, decl: *lir.Inst.Decl) !void {
        try self.gen(decl.value);
        try self.emit(if (decl.scope == .global) .global_set else .local_set);
        try self.emitUnsigned(decl.index);
    }

    /// Emits Wasm to perform a while loop. This first creates a block type
    /// and then a loop type
    fn emitWhile(self: *Wasm, loop: *lir.Inst.Double) !void {
        try self.emit(.block);
        try self.raw(Types.block);

        try self.emit(.loop);
        try self.raw(Types.block);

        // condition of the while loop
        try self.gen(loop.lhs);

        try self.emit(.break_if);
        try self.emitSigned(1);

        // block
        try self.gen(loop.rhs);

        try self.emit(.@"break");
        try self.emitSigned(0);

        // end loop
        try self.emit(.end);

        // end block
        try self.emit(.end);
    }

    /// Emits Wasm for an if-statement
    fn emitCond(self: *Wasm, cond: *lir.Inst.Condition) !void {
        // if block
        {
            try self.emit(.block);
            try self.raw(Types.block);

            try self.gen(cond.cond);

            try self.emit(.break_if);
            try self.emitSigned(0);

            try self.gen(cond.then_block);
            try self.emit(.end);
        }

        // else block
        {
            try self.emit(.block);
            try self.raw(Types.block);

            try self.gen(cond.cond);

            try self.emit(.break_if);
            try self.emitSigned(0);

            try self.gen(cond.else_block);
            try self.emit(.block);
        }
    }

    /// Emits a Wasm function, expects a declaration instead of a function
    /// as we need information regarding its index
    fn emitFunc(self: *Wasm, decl: *lir.Inst.Decl) !void {
        const func = decl.value.as(lir.Inst.Function);
        const name = decl.name;
    }
};

/// Emits a function type and appends it to the given section
/// Caller must ensure declaration contains a function
fn emitFuncType(section: *Section, func: *lir.Inst.Function) !void {
    const writer = section.code.writer();

    // tell wasm it's a function type
    try writer.writeByte(Types.func);

    // emit arguments length and their types
    try leb.writeULEB128(writer, @intCast(u32, func.args.len));
    for (func.args) |arg| try writer.writeByte(try Wasm.resolveValType(arg.ty));

    // Result types -> Wasm only allows for 1 result type, currently
    // if return type is void, provide no return type
    if (func.ret_type.ty == ._void) {
        try leb.writeULEB128(writer, @as(u32, 0));
    } else {
        try leb.writeULEB128(writer, @as(u32, 1));
        try writer.writeByte(try resolveValType(func.ret_type.ty));
    }

    // Make sure we increase the size of the section
    section.size += 1;
}

comptime {
    std.meta.refAllDecls(@This());
}
