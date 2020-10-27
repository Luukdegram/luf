pub const std = @import("std");
pub const lir = @import("ir.zig");
pub const LufType = @import("value.zig").Value.Type;
const Allocator = std.mem.Allocator;
const leb = std.debug.leb;
const testing = std.testing;

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
    i32_eqz = 0x45,
    i32_eq = 0x46,
    i32_ne = 0x47,
    i32_lt_s = 0x48,
    i64_eqz = 0x50,
    i64_eq = 0x51,
    i64_ne = 0x52,
    i64_lt = 0x53,
    i64_gt = 0x55,
    i64_le = 0x57,
    i64_ge = 0x59,
    i64_add = 0x7C,
    i64_sub = 0x7D,
    i64_mul = 0x7E,
    i64_div = 0x7F,
    i64_rem = 0x81,
    i64_and = 0x83,
    i64_or = 0x84,
    i64_xor = 0x85,
    i64_shl = 0x86,
    i64_shr = 0x87,

    /// Generates the Opcode for specific tag and its type
    /// i.e. generates .64_add if `wanted` = .add and `ty` = .integer
    fn fromTagAndType(wanted: lir.Inst.Tag, ty: LufType) Op {
        return switch (wanted) {
            .add => .i64_add,
            .sub => .i64_sub,
            .mul => .i64_mul,
            .div => .i64_div,
            .eql => .i64_eq,
            .nql => .i64_ne,
            .lt => .i64_lt,
            .gt => .i64_gt,
            .eql_lt => .i64_le,
            .eql_gt => .i64_ge,
            .bitwise_xor => .i64_xor,
            .bitwise_or => .i64_or,
            .bitwise_and => .i64_and,
            .shift_left => .i64_shl,
            .shift_right => .i64_shr,
            .mod => .i64_rem,
            else => unreachable,
        };
    }
};

/// Section id's as described at:
/// https://webassembly.github.io/spec/core/binary/modules.html#sections
const SectionType = enum {
    custom = 0,
    @"type" = 1,
    import = 2, // not used in Luf, everything is compiled to 1 compile unit
    func = 3,
    table = 4,
    memory = 5,
    global = 6,
    @"export" = 7,
    start = 8,
    element = 9,
    code = 10,
    data = 11,
};

const Section = struct {
    ty: SectionType,
    code: std.ArrayList(u8),
    count: u32,

    fn init(ty: SectionType, gpa: *Allocator) Section {
        return .{ .ty = ty, .code = std.ArrayList(u8).init(gpa), .count = 0 };
    }

    /// Emits all code from the section into the writer
    /// This invalidates the code saved into section itself
    fn emit(self: *Section, writer: anytype) !void {
        if (self.count == 0) return;

        // section id
        try leb.writeULEB128(writer, @enumToInt(self.ty));

        // full payload length. + 1 for `count`
        try leb.writeULEB128(writer, @intCast(u32, self.code.items.len + 1));
        try leb.writeULEB128(writer, self.count);

        try writer.writeAll(self.code.items);
        self.code.deinit();
    }
};

/// Contains all possible types as described at
/// https://webassembly.github.io/spec/core/binary/types.html
const Types = struct {
    const block: i7 = -0x40; // we only allow void blocks
    const func: i7 = -0x20;
    const table: u8 = 0x70;

    /// Limits as described at:
    /// https://webassembly.github.io/spec/core/binary/types.html#limits
    const Limits = enum(u1) {
        zero,
        one,
    };

    /// Wasm Value types as described at:
    /// https://webassembly.github.io/spec/core/binary/types.html#value-types
    const Value = enum(i7) {
        I32 = -0x01,
        I64 = -0x02,
        F32 = -0x03,
        F64 = -0x04,
    };

    /// Export sections as described at:
    /// http://webassembly.github.io/spec/core/binary/modules.html#export-section
    const Export = enum(u7) {
        func = 0,
        table = 1,
        mem = 2,
        global = 3,
    };
};

// Magic constants, required for a valid Wasm module
const module_header = [_]u8{ 0x00, 0x61, 0x73, 0x6D }; // \x00asm
const module_version = [_]u8{ 0x01, 0x00, 0x00, 0x00 }; // v1

/// Utility struct with helper functions to make it easier
/// to manage instructions
pub const Wasm = struct {
    buffer: std.ArrayList(u8),
    sections: [12]Section,
    section_size: usize,
    gpa: *Allocator,

    pub const Error = error{OutOfMemory};

    /// Creates a new instance of `Instructions`
    pub fn init(gpa: *Allocator) Wasm {
        return .{
            .buffer = std.ArrayList(u8).init(gpa),
            .sections = undefined,
            .section_size = 0,
            .gpa = gpa,
        };
    }

    /// Creates a new instance of `Instructions` aswell as codegen bytecode instructions from Luf's IR
    pub fn fromCu(gpa: *Allocator, cu: lir.CompileUnit) Error![]const u8 {
        var wasm = init(gpa);
        const writer = wasm.buffer.writer();

        try writer.writeAll(module_header[0..]);
        try writer.writeAll(module_version[0..]);

        // first do all declarations, required for our sections
        for (cu.instructions) |inst| {
            if (inst.tag == .decl and inst.as(lir.Inst.Decl).value.tag == .func) {
                try wasm.emitFunc(inst.as(lir.Inst.Decl));
            }
        }

        // sort our sections so they will be emitted in correct order
        std.sort.sort(Section, wasm.sections[0..wasm.section_size], {}, sortSections);

        // emit the sections
        for (wasm.sections[0..wasm.section_size]) |*sec| {
            try sec.emit(writer);
        }

        // Finally, do all other instructions
        for (cu.instructions) |inst| {
            if (inst.tag == .decl) continue;
            try wasm.gen(writer, inst);
        }

        return wasm.final();
    }

    /// Returns a section based on the given section type
    /// if the section does not exist yet, a new one will be created
    fn section(self: *Wasm, id: SectionType) *Section {
        for (self.sections[0..self.section_size]) |*s| if (s.ty == id) return s;

        // not found, so create a new Section and return a pointer to it
        self.sections[self.section_size] = Section.init(id, self.gpa);
        defer self.section_size += 1;
        return &self.sections[self.section_size];
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

    /// Resolves the Wasm's value type given the LufType
    fn resolveValType(ty: LufType) Types.Value {
        return switch (ty) {
            .integer => Types.Value.I64,
            else => @panic("TODO: Implement more types for wasm"),
        };
    }

    /// Generates and appends instructions based on the given IR instruction
    fn gen(self: *Wasm, writer: anytype, inst: *lir.Inst) Error!void {
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
            => try self.emitInfix(writer, inst.as(lir.Inst.Double)),
            .not, .bitwise_not, .negate => {},
            .int => try self.emitInt(writer, inst.as(lir.Inst.Int)),
            .string => {},
            .primitive => {},
            .ident => try self.emitIdent(writer, inst.as(lir.Inst.Ident)),
            .expr => try self.gen(writer, inst.as(lir.Inst.Single).rhs),
            .decl => try self.emitDecl(writer, inst.as(lir.Inst.Decl)),
            .@"return" => try self.emitRet(writer, inst.as(lir.Inst.Single)),
            .assign => {},
            .store => {},
            .load => {},
            .list, .map => {},
            .pair => {},
            .range => {},
            .import => {},
            .@"enum" => {},
            .condition => try self.emitCond(writer, inst.as(lir.Inst.Condition)),
            .block => try self.emitBlock(writer, inst.as(lir.Inst.Block)),
            .func => {},
            .call => try self.emitCall(writer, inst.as(lir.Inst.Call)),
            .@"while" => try self.emitWhile(writer, inst.as(lir.Inst.Double)),
            .@"switch" => {},
            .branch => {},
            .@"break" => {},
            .@"continue" => {},
            .@"for" => {},
            .comment, .type_def, .func_arg => {}, //ignore those
        }
    }

    /// Writes an unsigned integer value
    fn emitUnsigned(self: *Wasm, writer: anytype, value: anytype) !void {
        try leb.writeULEB128(writer, value);
    }

    /// Writes a signed integer value
    fn emitSigned(self: *Wasm, writer: anytype, value: anytype) !void {
        try leb.writeILEB128(writer, value);
    }

    /// Emits an integer
    fn emitInt(self: *Wasm, writer: anytype, int: *lir.Inst.Int) !void {
        try self.emit(writer, .i64_const);
        try self.emitSigned(writer, int.value);
    }

    /// Emits a single opcode
    fn emit(self: *Wasm, writer: anytype, op: Op) !void {
        try writer.writeByte(@enumToInt(op));
    }

    /// Emits a binary operator opcode based on the given types
    fn emitInfix(self: *Wasm, writer: anytype, double: *lir.Inst.Double) !void {
        try self.gen(writer, double.lhs);
        try self.gen(writer, double.rhs);

        switch (double.base.tag) {
            .assign_add, .assign_sub, .assign_mul, .assign_div, .@"and", .@"or" => @panic("TODO Add support for assignments and logical AND/OR"),
            else => try self.emit(writer, Op.fromTagAndType(double.base.tag, double.lhs.ty)),
        }
        //try self.emit(writer, .i64_add);
    }

    /// Emits a Wasm block instruction
    fn emitBlock(self: *Wasm, writer: anytype, block: *lir.Inst.Block) !void {
        for (block.instructions) |inst| try self.gen(writer, inst);
    }

    /// Loads a local or global variable onto the stack
    fn emitIdent(self: *Wasm, writer: anytype, ident: *lir.Inst.Ident) !void {
        try self.emit(writer, if (ident.scope == .global) Op.global_get else Op.local_get);
        try self.emitUnsigned(writer, ident.index);
    }

    /// Emits a return statement in Wasm
    fn emitRet(self: *Wasm, writer: anytype, ret: *lir.Inst.Single) !void {
        try self.gen(writer, ret.rhs);
        try self.emit(writer, .@"return");
    }

    /// Emits wasm for the declation's value to put it on the stack
    /// and then generates a .local_set or .global_set based on its scope
    fn emitDecl(self: *Wasm, writer: anytype, decl: *lir.Inst.Decl) !void {
        try self.gen(writer, decl.value);
        try self.emit(writer, if (decl.scope == .global) Op.global_set else Op.local_set);
        try self.emitUnsigned(writer, decl.index);
    }

    /// Emits Wasm to perform a while loop. This first creates a block type
    /// and then a loop type
    fn emitWhile(self: *Wasm, writer: anytype, loop: *lir.Inst.Double) !void {}

    /// Emits Wasm for an if-statement
    fn emitCond(self: *Wasm, writer: anytype, cond: *lir.Inst.Condition) !void {
        // start our block of expressions
        try self.emit(writer, .block);
        try self.emitSigned(writer, Types.block); // void block

        // generate the condition to determine if or else
        try self.gen(writer, cond.cond);

        // start our if statement with an implicit 'then block'
        try self.emit(writer, .@"if");
        try self.emitSigned(writer, Types.block);
        try self.gen(writer, cond.then_block);

        if (cond.else_block) |alt| {
            try self.emit(writer, .@"else");
            try self.gen(writer, alt);
        }

        // end our if statement
        try self.emit(writer, .end);

        // end our block
        try self.emit(writer, .end);
    }

    /// Emits Wasm bytecode to call a function
    fn emitCall(self: *Wasm, writer: anytype, call: *lir.Inst.Call) !void {
        // generate arguments
        for (call.args) |arg| try self.gen(writer, arg);

        // find funcidx and emit .call (0x10)
        const ident = call.func.as(lir.Inst.Ident);
        try self.emit(writer, .call);
        try self.emitUnsigned(writer, ident.index);
    }

    /// Emits a Wasm function, expects a declaration instead of a function
    /// as we need information regarding its index and name
    fn emitFunc(self: *Wasm, decl: *lir.Inst.Decl) !void {
        const func = decl.value.as(lir.Inst.Function);
        const name = decl.name;

        // register the function type
        const type_section = self.section(.@"type");
        const type_idx = try emitFuncType(type_section, func);

        // register the function itself using the type idx created above
        const func_section = self.section(.func);
        const func_idx = func_section.count;
        try leb.writeULEB128(func_section.code.writer(), func_idx);
        func_section.count += 1; // manually increase as no helper function here

        // register the code section, this will contain the body of the function
        const code_section = self.section(.code);
        try self.emitFuncBody(code_section, func);

        // export the function to make it available to other modules/browser
        // TODO, make this dependent on decl.is_pub
        const export_section = self.section(.@"export");
        try exportFunc(export_section, decl);
    }

    /// Appends a function body into the given section
    /// Will increase the section's count by 1
    fn emitFuncBody(self: *Wasm, sec: *Section, func: *lir.Inst.Function) !void {
        var func_body = std.ArrayList(u8).init(self.gpa);
        defer func_body.deinit();
        const writer = func_body.writer();

        // calculate locals (Don't count arguments as locals)
        const locals = @intCast(u32, func.locals - func.args.len);
        try leb.writeULEB128(writer, locals);

        const body = func.body.as(lir.Inst.Block);
        // if we have locals, extract them from the body
        // TODO maybe we can make this nicer by making locals a slice of instructions
        // rather than a counter so we don't have to loop over the body twice
        // TODO we need to tell how many we have per type, yikes
        if (locals > 0) {
            for (body.instructions) |inst| {
                if (inst.tag == .decl) {
                    try leb.writeILEB128(writer, @enumToInt(resolveValType(inst.ty)));
                }
            }
        }

        // generate the bytecode for the body
        // exclude the declarations
        for (body.instructions) |inst| {
            if (inst.tag != .decl) try self.gen(writer, inst);
        }

        // "end" byte
        try self.emit(writer, .end);

        const sec_writer = sec.code.writer();

        try leb.writeULEB128(sec_writer, @intCast(u32, func_body.items.len));
        try sec_writer.writeAll(func_body.items);

        sec.count += 1;
    }
};

/// Emits a function type and appends it to the given section
/// Returns the typeidx and increases the section's count by 1
fn emitFuncType(sec: *Section, func: *lir.Inst.Function) !u32 {
    const writer = sec.code.writer();

    // tell wasm it's a function type
    try leb.writeILEB128(writer, Types.func);

    // emit arguments length and their types
    try leb.writeULEB128(writer, @intCast(u32, func.args.len));
    for (func.args) |arg| try leb.writeILEB128(writer, @enumToInt(Wasm.resolveValType(arg.ty)));

    // Result types -> Wasm only allows for 1 result type, currently
    // if return type is void, provide no return type
    if (func.ret_type.ty == ._void) {
        try leb.writeULEB128(writer, @as(u1, 0));
    } else {
        try leb.writeULEB128(writer, @as(u1, 1));
        const ret_type = Wasm.resolveValType(func.ret_type.ty);
        try leb.writeILEB128(writer, @enumToInt(ret_type));
    }

    // Make sure we increase the size of the section
    sec.count += 1;

    // the typeidx (-1 because we just increased it by 1 above)
    return sec.count - 1;
}

/// Emits a function into the 'export' section
fn exportFunc(sec: *Section, decl: *lir.Inst.Decl) !void {
    const writer = sec.code.writer();
    try leb.writeULEB128(writer, @intCast(u32, decl.name.len));
    try writer.writeAll(decl.name);
    try writer.writeByte(@enumToInt(Types.Export.func));
    try leb.writeULEB128(writer, decl.index);

    sec.count += 1;
}

/// function used to sort sections by their type
fn sortSections(context: void, lhs: Section, rhs: Section) bool {
    return @enumToInt(lhs.ty) < @enumToInt(rhs.ty);
}

comptime {
    std.meta.refAllDecls(@This());
}

/// compiles input and checks if it matches the expected output
fn testWasm(input: []const u8, expected: []const u8) !void {
    const alloc = testing.allocator;
    var err = @import("error.zig").Errors.init(alloc);
    defer err.deinit();

    var cu = try @import("compiler.zig").compile(alloc, input, &err);
    defer cu.deinit();

    const wasm = try Wasm.fromCu(alloc, cu);
    defer alloc.free(wasm);

    testing.expectEqualStrings(expected, wasm);
}

const magic_bytes = &[_]u8{ 0, 'a', 's', 'm', 1, 0, 0, 0 };

test "IR to Wasm - Basic" {
    const input = "1 + 1";
    const expected = magic_bytes ++ "\x42\x01\x42\x01\x7c";

    try testWasm(input, expected);
}

test "IR to Wasm - Functions" {
    const input = "const add = fn(x: int, y: int) int { return x + y }";

    const expected = magic_bytes ++ // \0asm                (module
        "\x01\x07\x01\x60\x02\x7e\x7e\x01\x7e" ++ //            (type (i64 i64) (func (result i64)))
        "\x03\x02\x01\x00\x07" ++ //                            (func (i64 i64) (type 0))
        "\x07\x01\x03\x61\x64\x64\x00\x00" ++ //                (export "add" (func 0))
        "\x0a\x0a\x01\x08\x00\x20\x00\x20\x01\x7c\x0f\x0b"; //      load_local load_local i64.add)

    try testWasm(input, expected);
}

test "IR to Wasm - Conditionals" {
    const input =
        \\const test = fn(x: int) int { 
        \\  if (x == 2) {
        \\      return 5
        \\  } else {
        \\      return 10
        \\  }
        \\  return 15
        \\}
    ;
    const alloc = testing.allocator;
    var err = @import("error.zig").Errors.init(alloc);
    defer err.deinit();

    var cu = try @import("compiler.zig").compile(alloc, input, &err);
    defer cu.deinit();

    const wasm = try Wasm.fromCu(alloc, cu);
    defer alloc.free(wasm);

    var file = try std.fs.cwd().createFile("src/cond.wasm", .{});
    defer file.close();

    try file.writeAll(wasm);
}
