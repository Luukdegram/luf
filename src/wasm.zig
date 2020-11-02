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
    i32_const = 0x41,
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
            .add, .assign_add => .i64_add,
            .sub, .assign_sub => .i64_sub,
            .mul, .assign_mul => .i64_mul,
            .div, .assign_div => .i64_div,
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
    /// `SectionType` that is represented by this `Section`
    ty: SectionType,
    /// The full bytecode contained within this section.
    /// use emit() to transfer to another writer
    code: std.ArrayList(u8),
    /// The amount of additions done to this section.
    /// i.e. the amount of function bodies inside the 'code section'
    count: u32,

    /// Initializes a section
    fn init(ty: SectionType, gpa: *Allocator) Section {
        return .{ .ty = ty, .code = std.ArrayList(u8).init(gpa), .count = 0 };
    }

    /// Emits all code from the section into the writer
    /// This invalidates the `code` saved into section itself
    fn emit(self: *Section, writer: anytype) !void {
        if (self.count == 0) return;

        // section id
        try leb.writeULEB128(writer, @enumToInt(self.ty));

        // start section always has 1 element, therefore no need to emit counter
        if (self.ty == .start) {
            try leb.writeULEB128(writer, @intCast(u32, self.code.items.len));
        } else {
            // full payload length. + 1 for `count`
            try leb.writeULEB128(writer, @intCast(u32, self.code.items.len + 1));
            try leb.writeULEB128(writer, self.count);
        }

        try writer.writeAll(self.code.items);
        self.code.deinit();
    }
};

/// Contains all possible types as described at
/// https://webassembly.github.io/spec/core/binary/types.html
const Types = struct {
    const block: i7 = -0x40; // void block
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

        /// returns actual value of the enum
        /// shorthand for @enumToInt as a method instead
        fn val(self: Value) i7 {
            return @enumToInt(self);
        }
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
const module_header = [_]u8{ 0x00, 0x61, 0x73, 0x6D }; // 0x00asm
const module_version = [_]u8{ 0x01, 0x00, 0x00, 0x00 }; // v1

/// Utility struct with helper functions to make it easier
/// to manage instructions
pub const Wasm = struct {
    /// buffer that will contain all instructions
    buffer: std.ArrayList(u8),
    /// array of all sections that are optional and may contain instructions
    sections: [12]Section,
    /// size of the section list that have been initialized. (`sections` will be sorted)
    section_size: usize,
    /// allocator used to generate the instructions
    gpa: *Allocator,
    /// current function, used to check for return types in blocks
    func: *lir.Inst.Function,
    /// funcidx of the main function. Cannot be 'null' after iterating our declarations
    main_index: ?usize,
    /// Struct to save strings for use in Data section
    data: Data,

    /// Struct to handle strings inside data section
    /// returns the offset of a particular string if found
    const Data = struct {
        /// map of strings as key and offset as value
        strings: std.StringHashMapUnmanaged(i32),
        /// wasm offset expects i32
        offset: i32,

        /// Returns `true` if the string already exists
        fn contains(self: Data, key: []const u8) bool {
            return self.strings.contains(key);
        }

        /// Returns a string's offset if found, else returns null
        fn get(self: Data, key: []const u8) ?u32 {
            return self.strings.get(key);
        }

        /// Puts a new key string into `strings`, asserts key does not exist
        fn put(self: *Data, gpa: *Allocator, key: []const u8) !void {
            try self.strings.putNoClobber(gpa, key, self.offset);
            self.offset += @intCast(i32, key.len);
        }
    };

    /// Errors emitted while generating the Wasm bytecode
    pub const Error = error{
        OutOfMemory,
        InvalidType,
        ParametersDisallowed,
    };

    /// Creates a new instance of `Instructions`
    pub fn init(gpa: *Allocator) Wasm {
        return .{
            .buffer = std.ArrayList(u8).init(gpa),
            .sections = undefined,
            .section_size = 0,
            .gpa = gpa,
            .func = undefined,
            .main_index = null,
            .data = .{
                .strings = std.StringHashMapUnmanaged(i32){},
                .offset = 0,
            },
        };
    }

    /// Creates a new instance of `Instructions` aswell as codegen bytecode instructions from Luf's IR
    pub fn fromCu(gpa: *Allocator, cu: lir.CompileUnit) Error![]const u8 {
        var wasm = init(gpa);
        const writer = wasm.buffer.writer();

        try writer.writeAll(module_header[0..]);
        try writer.writeAll(module_version[0..]);

        // build the bytecode
        for (cu.instructions) |inst| {
            if (inst.tag != .decl) continue; // only declarations

            const decl = inst.as(lir.Inst.Decl);
            if (decl.value.tag == .func)
                try wasm.emitFunc(inst.as(lir.Inst.Decl))
            else
                try wasm.emitGlobal(decl);
        }

        // if main function defined, insert it into start section
        if (wasm.main_index) |start_index| {
            const start = wasm.section(.start);
            try wasm.emitUnsigned(start.code.writer(), start_index);
            start.count += 1;
        }

        // sort our sections so they will be emitted in correct order
        std.sort.sort(Section, wasm.sections[0..wasm.section_size], {}, sortSections);

        // emit the sections into the final bytecode
        for (wasm.sections[0..wasm.section_size]) |*sec| {
            try sec.emit(writer);
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
            .boolean => Types.Value.I32,
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
            .not, .bitwise_not, .negate => try self.emitPrefix(writer, inst.as(lir.Inst.Single)),
            .int => try self.emitInt(writer, inst.as(lir.Inst.Int)),
            .string => try self.emitString(inst.as(lir.Inst.String)),
            .primitive => {},
            .ident => try self.emitIdent(writer, inst.as(lir.Inst.Ident)),
            .expr => try self.gen(writer, inst.as(lir.Inst.Single).rhs),
            .decl => try self.emitDecl(writer, inst.as(lir.Inst.Decl)),
            .@"return" => try self.emitRet(writer, inst.as(lir.Inst.Single)),
            .assign,
            .assign_add,
            .assign_sub,
            .assign_mul,
            .assign_div,
            => try self.emitAssign(writer, inst.as(lir.Inst.Double)),
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
        try self.emitSigned(writer, @intCast(i64, int.value));
    }

    /// Saves the string in the Data section
    fn emitString(self: *Wasm, string: *lir.Inst.String) !void {
        if (self.data.contains(string.value)) return;

        // string does not exist yet, so add it to our data section, as well as to
        // our data struct
        const data = self.section(.data);
        const writer = data.code.writer();

        // index is always 0
        try self.emitUnsigned(writer, @as(u32, 0));

        // offset
        try self.emit(writer, .i32_const);
        try self.emitSigned(writer, self.data.offset);

        // length + value
        try self.emitUnsigned(writer, @intCast(u32, string.value.len));
        try writer.writeAll(string.value);
        data.count += 1;

        // save it in our data struct to calculate the offset for later
        try self.data.put(self.gpa, string.value);
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
            .@"and", .@"or" => @panic("TODO Add support for logical AND/OR"),
            else => try self.emit(writer, Op.fromTagAndType(double.base.tag, double.lhs.ty)),
        }
    }

    /// Emits a prefix operator such as negate, mod, etc
    fn emitPrefix(self: *Wasm, writer: anytype, single: *lir.Inst.Single) !void {
        // Wasm does not have bytecodes for prefix operands
        // therefore we apply it manually
        switch (single.base.tag) {
            .negate => {
                try self.emit(writer, .i64_const);
                try self.emitSigned(writer, @as(i64, 0));
                try self.gen(writer, single.rhs);
                try self.emit(writer, .i64_sub);
            },
            .not => {
                try self.gen(writer, single.rhs);
                try self.emit(writer, .i32_eqz);
            },
            .bitwise_not => {
                try self.gen(writer, single.rhs);
                try self.emit(writer, .i64_const);
                try self.emitSigned(writer, @as(i64, -1));
                try self.emit(writer, .i64_xor);
            },
            else => unreachable,
        }
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

    /// Emits bytecode that first retrieves the local or global and then assigns
    /// a new value to it
    fn emitAssign(self: *Wasm, writer: anytype, double: *lir.Inst.Double) !void {
        const ident = double.lhs.as(lir.Inst.Ident);

        // get the local/global
        try self.gen(writer, double.lhs);

        // generate the new value
        try self.gen(writer, double.rhs);

        // if operator, first apply it
        switch (double.base.tag) {
            .assign => {},
            else => try self.emit(writer, Op.fromTagAndType(double.base.tag, ident.base.ty)),
        }

        // set the new value to the local/global
        try self.emit(writer, if (ident.scope == .global) Op.global_set else Op.local_set);
        try self.emitUnsigned(writer, ident.index);
    }

    /// Emits wasm for the declation's value to put it on the stack
    /// and then generates a .local_set or .global_set based on its scope
    fn emitDecl(self: *Wasm, writer: anytype, decl: *lir.Inst.Decl) !void {
        try self.gen(writer, decl.value);
        try self.emit(writer, .local_set); // globals are done seperately, therefore this is a local
        try self.emitUnsigned(writer, decl.index);
    }

    /// Emits Wasm to perform a while loop. This first creates a block type
    /// and then a loop type
    fn emitWhile(self: *Wasm, writer: anytype, loop: *lir.Inst.Double) !void {
        // begin block
        try self.emit(writer, .block);
        try self.emitSigned(writer, Types.block);

        // begin loop
        try self.emit(writer, .loop);
        try self.emitSigned(writer, Types.block);

        // generate the condition
        try self.gen(writer, loop.lhs);

        // break loop if condition = false
        try self.emit(writer, .i32_eqz);
        try self.emit(writer, .break_if);
        try self.emitUnsigned(writer, @as(u32, 1));

        // finally, generate its body
        try self.gen(writer, loop.rhs);

        // continue at loop label
        try self.emit(writer, .@"break");
        try self.emitUnsigned(writer, @as(u32, 0));

        // end loop
        try self.emit(writer, .end);

        // end block
        try self.emit(writer, .end);
    }

    /// Emits Wasm for an if-statement
    fn emitCond(self: *Wasm, writer: anytype, cond: *lir.Inst.Condition) !void {
        // generate the condition to determine if or else
        try self.gen(writer, cond.cond);

        // start our if statement with an implicit 'then block'
        try self.emit(writer, .@"if");
        if (self.func.ret_type.ty == ._void)
            try self.emitSigned(writer, Types.block)
        else
            try self.emitSigned(writer, resolveValType(self.func.ret_type.ty).val());
        try self.gen(writer, cond.then_block);

        if (cond.else_block) |alt| {
            try self.emit(writer, .@"else");
            try self.gen(writer, alt);
        }

        // end our if statement
        try self.emit(writer, .end);
    }

    /// Emits the bytecode for a global variable by appending it to the
    /// 'global' section of the module
    fn emitGlobal(self: *Wasm, decl: *lir.Inst.Decl) !void {
        const sec = self.section(.global);

        const writer = sec.code.writer();
        // emit its type
        try self.emitSigned(writer, resolveValType(decl.value.ty).val());
        // mutability: 0 immutable, 1 mutable
        try self.emitUnsigned(writer, @boolToInt(decl.is_mut));

        // TODO: For now we only support init expr for integers
        if (decl.value.ty == .integer)
            try self.emitInt(writer, decl.value.as(lir.Inst.Int))
        else {
            // for now just emit a 0 value
            try self.emit(writer, .i64_const);
            try self.emitSigned(writer, @as(i64, 0));
        }

        // emit 'end' so wasm is aware where our global ends
        try self.emit(writer, .end);

        // TODO: Save the globalidx somewhere
        sec.count += 1;
    }

    /// Emits Wasm bytecode to call a function
    fn emitCall(self: *Wasm, writer: anytype, call: *lir.Inst.Call) !void {
        // generate arguments
        for (call.args) |arg| try self.gen(writer, arg);

        // find funcidx and emit .call (0x10)
        const ident = call.func.as(lir.Inst.Ident);
        try self.emit(writer, .call);

        // functions are compiled first by the compiler, therefore their indices
        // will match that of Wasm's funcidx's. This means we can directly use the index
        try self.emitUnsigned(writer, ident.index);
    }

    /// Emits a Wasm function, expects a declaration instead of a function
    /// as we need information regarding its index and name
    fn emitFunc(self: *Wasm, decl: *lir.Inst.Decl) !void {
        const func = decl.value.as(lir.Inst.Function);
        const name = decl.name;
        self.func = func;

        // if main is declared, ensure no parameters and return type are set
        const is_main = if (std.mem.eql(u8, "main", name)) blk: {
            if (func.ret_type.ty != ._void) return Error.InvalidType;
            if (func.args.len > 0) return Error.ParametersDisallowed;

            break :blk true;
        } else false;

        // register the function type
        const type_section = self.section(.@"type");
        const type_idx = try emitFuncType(type_section, func);

        // register the function itself using the type idx created above
        const func_section = self.section(.func);
        const func_idx = func_section.count;
        try leb.writeULEB128(func_section.code.writer(), func_idx);
        func_section.count += 1; // manually increase as no helper function here

        if (is_main) self.main_index = func_idx;

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

        // generate bytecode for locals
        if (func.locals.len - func.args.len > 0) {
            var locals_map = std.AutoArrayHashMap(Types.Value, u32).init(self.gpa);
            defer locals_map.deinit();

            for (func.locals[func.args.len..]) |local| {
                const ty = resolveValType(local.ty);
                const entry = try locals_map.getOrPut(ty);
                if (entry.found_existing)
                    entry.entry.value += 1
                else
                    entry.entry.value = 1;
            }

            // write the locals (we actually emit the amount of types)
            try self.emitUnsigned(writer, locals_map.items().len);

            for (locals_map.items()) |entry| {
                try self.emitUnsigned(writer, entry.value);
                try self.emitSigned(writer, entry.key.val());
            }
        } else try self.emitUnsigned(writer, @as(u32, 0));

        const body = func.body.as(lir.Inst.Block);
        // generate the bytecode for the body
        for (body.instructions) |inst| {
            try self.gen(writer, inst);
        }

        // "end" byte that concludes the function body
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
    for (func.args) |arg| try leb.writeILEB128(writer, Wasm.resolveValType(arg.ty).val());

    // Result types -> Wasm only allows for 1 result type, currently
    // if return type is void, provide no return type
    if (func.ret_type.ty == ._void) {
        try leb.writeULEB128(writer, @as(u1, 0));
    } else {
        try leb.writeULEB128(writer, @as(u1, 1));
        try leb.writeILEB128(writer, Wasm.resolveValType(func.ret_type.ty).val());
    }

    // Make sure we increase the size of the section
    sec.count += 1;

    // the typeidx
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

/// When `file_name` is set to a value, it will generate a wasm binary file
/// which can be used by external tools to expect the output
const TestOutput = struct {
    file_name: ?[]const u8 = null,
    print_output: bool = false,
};

/// compiles input and checks if it matches the expected output
fn testWasm(input: []const u8, expected: []const u8, with_output: TestOutput) !void {
    const alloc = testing.allocator;
    var err = @import("error.zig").Errors.init(alloc);
    defer err.deinit();

    var cu = try @import("compiler.zig").compile(alloc, input, &err);
    defer cu.deinit();

    const wasm = try Wasm.fromCu(alloc, cu);
    defer alloc.free(wasm);

    if (with_output.file_name) |name| {
        var file = try std.fs.cwd().createFile(name, .{});
        defer file.close();
        try file.writeAll(wasm);
    }

    if (with_output.print_output) {
        for (wasm) |c| std.debug.print("\\x{x:0>2}", .{c});
        std.debug.print("\n", .{});
    }

    testing.expectEqualSlices(u8, expected, wasm);
}

const magic_bytes = &[_]u8{ 0, 'a', 's', 'm', 1, 0, 0, 0 };

test "IR to Wasm - Functions" {
    const input = "const add = fn(x: int, y: int) int { return x + y }";

    const expected = magic_bytes ++ // \0asm                (module
        "\x01\x07\x01\x60\x02\x7e\x7e\x01\x7e" ++ //            (type (i64 i64) (func (result i64)))
        "\x03\x02\x01\x00" ++ //                            (func (i64 i64) (type 0))
        "\x07\x07\x01\x03\x61\x64\x64\x00\x00" ++ //                (export "add" (func 0))
        "\x0a\x0a\x01\x08\x00\x20\x00\x20\x01\x7c\x0f\x0b"; //      load_local load_local i64.add)

    try testWasm(input, expected, .{});
}

test "IR to Wasm - Conditional" {
    const input =
        \\const con = fn(x: int) int { 
        \\  if (x == 2) {
        \\      return 5
        \\  } else {
        \\      return 10
        \\  }
        \\}
    ;

    const expected = magic_bytes ++
        "\x01\x06\x01\x60\x01\x7e\x01\x7e" ++
        "\x03\x02\x01\x00" ++
        "\x07\x07\x01\x03\x63\x6f\x6e\x00\x00" ++
        "\x0a\x13\x01\x11\x00\x20\x00\x42\x02\x51\x04\x7e\x42\x05\x0f\x05\x42\x0a\x0f\x0b\x0b";

    try testWasm(input, expected, .{});
}

test "IR to Wasm - Function locals" {
    const input =
        \\const loc = fn(x: int) int { 
        \\  const y = 20
        \\  if (x == 2) {
        \\      return 5
        \\  } else {
        \\      return 10
        \\  }
        \\}
    ;

    const expected = magic_bytes ++
        "\x01\x06\x01\x60\x01\x7e\x01\x7e" ++
        "\x03\x02\x01\x00" ++
        "\x07\x07\x01\x03\x6c\x6f\x63\x00\x00" ++
        "\x0a\x19\x01\x17\x01\x01\x7E\x42\x14\x21\x01" ++ // locals section
        "\x20\x00\x42\x02\x51\x04\x7e\x42\x05\x0f\x05\x42\x0a\x0f\x0b\x0b";

    try testWasm(input, expected, .{});
}

test "IR to Wasm - Globals" {
    const input =
        \\const x = 5
        \\const test = fn()void{}
    ;
    const expected = magic_bytes ++
        "\x01\x04\x01\x60\x00\x00" ++
        "\x03\x02\x01\x00" ++
        "\x06\x06\x01\x7e\x00\x42\x05\x0b" ++ // globals section
        "\x07\x08\x01\x04\x74\x65\x73\x74\x00\x00" ++
        "\x0a\x04\x01\x02\x00\x0b";

    try testWasm(input, expected, .{});
}

test "IR to Wasm - main func" {
    const input = "const main = fn()void{}";
    const expected = magic_bytes ++
        "\x01\x04\x01\x60\x00\x00" ++
        "\x03\x02\x01\x00" ++
        "\x07\x08\x01\x04\x6d\x61\x69\x6e\x00\x00" ++
        "\x08\x01\x00" ++ // start section
        "\x0a\x04\x01\x02\x00\x0b";

    try testWasm(input, expected, .{});
}

test "IR to Wasm - Function call" {
    const input =
        \\const addOne = fn(x: int) int {
        \\  return x + 1
        \\}
        \\const main = fn() void {
        \\  const x = addOne(1)
        \\}
    ;

    const expected = magic_bytes ++
        "\x01\x09\x02\x60\x01\x7e\x01\x7e\x60\x00\x00" ++
        "\x03\x03\x02\x00\x01" ++
        "\x07\x11\x02\x06\x61\x64\x64\x4f\x6e\x65\x00\x00\x04\x6d\x61\x69\x6e\x00\x01" ++
        "\x08\x01\x01\x0a\x15\x02\x08\x00\x20\x00\x42\x01\x7c\x0f\x0b" ++
        "\x0a\x01\x01\x7e\x42\x01\x10\x00\x21\x00\x0b";

    try testWasm(input, expected, .{});
}

test "IR to Wasm - Loop" {
    const input =
        \\const loop = fn() void {
        \\  const x = 5
        \\  mut i = 0
        \\  while(i < x) {
        \\      i = i + 1
        \\  }
        \\}
    ;

    const expected = magic_bytes ++
        "\x01\x04\x01\x60\x00\x00" ++
        "\x03\x02\x01\x00" ++
        "\x07\x08\x01\x04\x6c\x6f\x6f\x70\x00\x00" ++
        "\x0a\x27\x01\x25\x01\x02\x7e\x42\x05\x21\x00\x42\x00\x21\x01\x02\x40" ++
        "\x03\x40\x20\x01\x20\x00\x53\x45\x0d\x01\x20\x01\x20\x01\x42\x01\x7c\x21\x01\x0c\x00\x0b\x0b\x0b"; // loop starts at 0x03

    try testWasm(input, expected, .{});
}
