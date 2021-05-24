const std = @import("std");
const testing = std.testing;
const lir = @import("ir.zig");

/// Opcode for the virtual machine
pub const Opcode = enum(u8) {
    // general ops
    load_integer = 0,
    load_string = 1,
    load_func = 2,
    load_true = 4,
    load_false = 5,
    load_nil = 6,
    load_void = 7,
    load_global = 8,
    bind_global = 9,
    load_local = 10,
    bind_local = 11,
    assign_global = 12,
    assign_local = 13,
    jump_false = 14,
    jump = 15,
    call = 16,
    @"return" = 17,
    return_value = 18,
    load_module = 19,
    iter_next = 20,

    make_array = 21,
    make_map = 22,
    make_iter = 23,
    make_range = 24,
    make_enum = 54,
    make_slice = 55,

    //bin op
    add = 25,
    sub = 26,
    mul = 27,
    div = 28,
    mod = 29,
    equal = 30,
    not_equal = 31,
    greater_than = 32,
    greater_than_equal = 33,
    less_than = 34,
    less_than_equal = 35,
    minus = 36,
    not = 37,
    bitwise_or = 38,
    bitwise_xor = 39,
    bitwise_and = 40,
    bitwise_not = 41,
    shift_left = 42,
    shift_right = 43,
    @"and" = 44,
    @"or" = 45,
    assign_add = 46,
    assign_sub = 47,
    assign_mul = 48,
    assign_div = 49,
    // same as equal but does not pop the lhs value for switch prong
    match = 50,

    // referencing
    get_by_index = 51,
    set_by_index = 52,

    /// specifically removes a value from the stack
    pop = 53,
};

/// Utility struct with helper functions to make it easier
/// to manage instructions
pub const Instructions = struct {
    list: std.ArrayListUnmanaged(Instruction),
    gpa: *std.mem.Allocator,
    scope: Scope = Scope.none,

    pub const Error = error{OutOfMemory};

    const Id = enum { none, loop };

    const Scope = union(Id) {
        none,
        loop: struct { start: u32, jumps: std.ArrayListUnmanaged(u32) },

        /// Creates a new loop Scope
        fn createLoop(start: u32) Scope {
            return .{
                .loop = .{
                    .start = start,
                    .jumps = std.ArrayListUnmanaged(u32){},
                },
            };
        }
    };

    /// Creates a new instance of `Instructions`
    pub fn init(gpa: *std.mem.Allocator) Instructions {
        return .{
            .list = std.ArrayListUnmanaged(Instruction){},
            .gpa = gpa,
        };
    }

    /// Creates a new instance of `Instructions` aswell as codegen bytecode instructions from Luf's IR
    pub fn fromCu(gpa: *std.mem.Allocator, cu: lir.CompileUnit) Error!ByteCode {
        var instructions = init(gpa);

        for (cu.instructions) |inst| {
            try instructions.gen(inst);
        }

        return instructions.final();
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
            .slice => try self.emitSlice(inst.as(lir.Inst.Triple)),
            .comment, .type_def, .func_arg => {}, //VM doesn't do anything with this
        }
    }

    /// Return the amount of instructions
    fn len(self: Instructions) u32 {
        return @intCast(u32, self.list.items.len);
    }

    /// Appends a new `Instruction`
    fn append(self: *Instructions, inst: Instruction) !void {
        return self.list.append(self.gpa, inst);
    }

    /// Appends a new `Instruction` and returns the position of the instruction
    fn appendRetPos(self: *Instructions, inst: Instruction) !u32 {
        try self.list.append(self.gpa, inst);
        return self.len() - 1;
    }

    /// Can be used to check if last instruction is of given opcode
    fn lastIs(self: Instructions, op: Opcode) bool {
        const last = self.list.items[self.list.items.len - 1];
        return last.getOp() == op;
    }

    /// Replaces the ptr of an instruction, this is used for jumping instructions
    fn patch(self: *Instructions, pos: u32, ptr: u32) void {
        self.list.items[pos].ptr.pos = ptr;
    }

    /// Replaces the opcode of the last instruction
    /// Asserts there's atleast 1 instruction saved
    fn replaceLastOp(self: *Instructions, op: Opcode) void {
        std.debug.assert(self.list.items.len > 0);
        self.list.items[self.len() - 1].op = op;
    }

    /// Pops the last instruction
    fn pop(self: *Instructions) void {
        _ = self.list.popOrNull();
    }

    /// emits a single opcode
    fn emit(self: *Instructions, op: Opcode) !void {
        try self.append(Instruction.gen(op));
    }

    /// Emits an opcode that contains an aditional index/pointer to a length/object/position
    fn emitPtr(self: *Instructions, op: Opcode, ptr: u32) !void {
        try self.append(Instruction.genPtr(op, ptr));
    }

    /// Emits an opcode and returns a label with a pointer to the new instruction
    /// Sets the ptr to 0x0 as default
    fn label(self: *Instructions, op: Opcode) !u32 {
        return try self.appendRetPos(Instruction.genPtr(op, 0x0));
    }

    /// emits an integer
    fn emitInt(self: *Instructions, int: *lir.Inst.Int) !void {
        try self.append(Instruction.genInteger(int.value));
    }

    /// emits a string
    fn emitString(self: *Instructions, string: *lir.Inst.String) !void {
        try self.append(Instruction.genString(try self.gpa.dupe(u8, string.value)));
    }

    /// Emits a function
    fn emitFunc(self: *Instructions, name: []const u8, func: *lir.Inst.Function) !void {
        // initial jump over the body
        var jump = try self.label(.jump);
        const entry_point = self.len();

        try self.gen(func.body);

        // incase no return statement was inside the body, append our own that returns void
        if (!self.lastIs(.return_value)) try self.emit(.@"return");

        const end = try self.appendRetPos(Instruction.genFunction(
            try self.gpa.dupe(u8, name),
            func.locals.len,
            func.args.len,
            entry_point,
        ));

        // jump to end of our function
        self.patch(jump, end);
    }

    /// Generates bytecode for an arithmetic operation
    fn emitInfix(self: *Instructions, double: *lir.Inst.Double) !void {
        try self.gen(double.lhs);
        try self.gen(double.rhs);
        try self.emit(switch (double.base.tag) {
            .add => .add,
            .mul => .mul,
            .sub => .sub,
            .div => .div,
            .lt => .less_than,
            .gt => .greater_than,
            .eql => .equal,
            .nql => .not_equal,
            .eql_lt => .less_than_equal,
            .eql_gt => .greater_than_equal,
            .mod => .mod,
            .@"and" => .@"and",
            .@"or" => .@"or",
            .bitwise_xor => .bitwise_xor,
            .bitwise_or => .bitwise_or,
            .bitwise_and => .bitwise_and,
            .not => .bitwise_not,
            .shift_left => .shift_left,
            .shift_right => .shift_right,
            .assign_add => .assign_add,
            .assign_sub => .assign_sub,
            .assign_mul => .assign_mul,
            .assign_div => .assign_div,
            else => unreachable,
        });
    }

    /// Emits bytecode to load a prefix into the VM
    fn emitPrefix(self: *Instructions, single: *lir.Inst.Single) !void {
        try self.gen(single.rhs);
        try self.emit(switch (single.base.tag) {
            .negate => .minus,
            .not => .not,
            .bitwise_not => .bitwise_not,
            else => unreachable,
        });
    }

    /// Emits a primitive bytecode
    fn emitPrim(self: *Instructions, prim: *lir.Inst.Primitive) !void {
        try self.emit(switch (prim.prim_type) {
            .@"true" => .load_true,
            .@"false" => .load_false,
            .@"void" => .load_void,
            .nil => .load_nil,
        });
    }

    /// Generates the bytecode to load an identifier into the vm
    fn emitIdent(self: *Instructions, ident: *lir.Inst.Ident) !void {
        try self.emitPtr(
            if (ident.scope == .global) .load_global else .load_local,
            ident.index,
        );
    }

    /// First emits bytecode of expression's value, and then emits a `pop` instruction
    /// at the end of the expression.
    fn emitExpr(self: *Instructions, single: *lir.Inst.Single) !void {
        try self.gen(single.rhs);
        try self.emit(.pop);
    }

    /// Generates bytecode to bind a value to an identifier
    fn emitDecl(self: *Instructions, decl: *lir.Inst.Decl) !void {
        if (decl.value.tag == .func)
            try self.emitFunc(decl.name, decl.value.as(lir.Inst.Function))
        else
            try self.gen(decl.value);

        try self.emitPtr(
            if (decl.scope == .global) .bind_global else .bind_local,
            decl.index,
        );
    }

    /// Generates bytecode for returning a value
    fn emitRet(self: *Instructions, single: *lir.Inst.Single) !void {
        try self.gen(single.rhs);
        try self.emit(.return_value);
    }

    /// Generates bytecode to reassign a global or local variable
    fn emitAssign(self: *Instructions, double: *lir.Inst.Double) !void {
        const ident = double.lhs.as(lir.Inst.Ident);
        try self.gen(double.rhs);
        try self.emitPtr(
            if (ident.scope == .global) .assign_global else .assign_local,
            ident.index,
        );
    }

    /// Emits bytecode to assign a value to an element inside a map or list
    /// lhs is the list, index is the index to retrieve the element from the list
    /// and finally, rhs is the new value to assign to the element.
    fn emitStore(self: *Instructions, triple: *lir.Inst.Triple) !void {
        try self.gen(triple.lhs);
        try self.gen(triple.index);
        try self.gen(triple.rhs);
        try self.emit(.set_by_index);
    }

    /// Emits bytecode to retrieve an element from a map or list
    /// where lhs is the list and rhs is an index
    fn emitLoad(self: *Instructions, double: *lir.Inst.Double) !void {
        try self.gen(double.lhs);
        try self.gen(double.rhs);
        try self.emit(.get_by_index);
    }

    /// Generates bytecode from IR to create either a list or map
    fn emitList(self: *Instructions, ds: *lir.Inst.DataStructure) !void {
        for (ds.elements) |e| try self.gen(e);
        try self.emitPtr(
            if (ds.base.tag == .list) .make_array else .make_map,
            @intCast(u32, ds.elements.len),
        );
    }

    /// Emits bytecode to generate a key-value pair for maps
    fn emitPair(self: *Instructions, double: *lir.Inst.Double) !void {
        try self.gen(double.lhs);
        try self.gen(double.rhs);
    }

    /// Generates bytecode to create a range
    fn emitRange(self: *Instructions, double: *lir.Inst.Double) !void {
        try self.gen(double.lhs);
        try self.gen(double.rhs);
        try self.emit(.make_range);
    }

    /// Emits .load_module bytecode with filename of the imported module
    fn emitModule(self: *Instructions, string: *lir.Inst.String) !void {
        try self.emitString(string);
        try self.emit(.load_module);
    }

    /// Emits the bytecode required to build an enum
    fn emitEnum(self: *Instructions, enm: *lir.Inst.Enum) !void {
        for (enm.value) |e| try self.gen(e);
        try self.emitPtr(.make_enum, @intCast(u32, enm.value.len));
    }

    /// Emits the bytecode for an if with optional else statement
    fn emitCond(self: *Instructions, condition: *lir.Inst.Condition) !void {
        try self.gen(condition.cond);

        const false_label = try self.label(.jump_false);

        try self.gen(condition.then_block);

        if (self.lastIs(.pop)) self.pop();

        const jump_label = try self.label(.jump);

        self.patch(false_label, self.len());

        if (condition.else_block) |block| {
            try self.gen(block);

            if (self.lastIs(.pop)) self.pop();
        } else try self.emit(.load_void);

        self.patch(jump_label, self.len());
    }

    /// Generates bytecode for each expression inside the block
    /// Removes last instruction if it ends with a `pop`
    fn emitBlock(self: *Instructions, block: *lir.Inst.Block) !void {
        for (block.instructions) |inst| try self.gen(inst);

        if (self.lastIs(.pop))
            self.pop()
        else if (!self.lastIs(.return_value))
            try self.emit(.load_void);
    }

    /// Emits the .call bytecode after emitting the bytecode for the
    /// identifier
    fn emitCall(self: *Instructions, call: *lir.Inst.Call) !void {
        for (call.args) |arg| try self.gen(arg);
        try self.gen(call.func);

        try self.emitPtr(.call, @intCast(u32, call.args.len));
    }

    /// Generates the bytecode for a while loop
    fn emitWhile(self: *Instructions, loop: *lir.Inst.Double) !void {
        const start = self.len();

        const prev = self.scope;
        self.scope = Scope.createLoop(start);

        try self.gen(loop.lhs);
        const false_jump = try self.label(.jump_false);

        try self.gen(loop.rhs);

        try self.emit(.pop);

        try self.emitPtr(.jump, start);

        self.patch(false_jump, self.len());

        for (self.scope.loop.jumps.items) |jump_pos| {
            self.patch(jump_pos, self.len());
        }

        self.scope.loop.jumps.deinit(self.gpa);

        self.scope = prev;
    }

    /// Generates the full bytecode for a switch statement
    fn emitSwitch(self: *Instructions, sw: *lir.Inst.Switch) !void {
        try self.gen(sw.capture);
        for (sw.branches) |branch| try self.gen(branch);
        try self.emit(.pop);
    }

    /// Emits the bytecode for a single branch inside a switch statement
    fn emitBranch(self: *Instructions, branch: *lir.Inst.Double) !void {
        try self.gen(branch.lhs);
        try self.emit(.match);

        const jump = try self.label(.jump_false);
        try self.gen(branch.rhs);
        self.patch(jump, self.len());
    }

    /// Emits bytecode to generate a for loop
    fn emitLoop(self: *Instructions, loop: *lir.Inst.Loop) !void {
        // first create our iterator
        try self.gen(loop.it);
        try self.emitPtr(.make_iter, if (loop.index != null) 1 else 0);

        // start of loop
        try self.emit(.iter_next);

        const prev = self.scope;
        self.scope = Scope.createLoop(self.len() - 1);

        const end_jump = try self.label(.jump_false);

        // index and capture
        if (loop.index) |index| {
            try self.emitPtr(.assign_local, index.as(lir.Inst.Ident).index);
            try self.emit(.pop);
        }
        try self.emitPtr(.assign_local, loop.capture.as(lir.Inst.Ident).index);
        try self.emit(.pop);

        try self.gen(loop.block);

        // pop last value from block before we jump to ensure clean loop state
        if (!self.lastIs(.pop)) try self.emit(.pop);

        try self.emitPtr(.jump, self.scope.loop.start);

        self.patch(end_jump, self.len());

        for (self.scope.loop.jumps.items) |jump_pos| {
            self.patch(jump_pos, self.len());
        }
        if (self.scope.loop.jumps.items.len > 0) try self.emit(.pop);

        self.scope.loop.jumps.deinit(self.gpa);

        self.scope = prev;
    }

    /// Generates the bytecode to create a slice from a string
    pub fn emitSlice(self: *Instructions, slice: *lir.Inst.Triple) !void {
        try self.gen(slice.lhs);
        try self.gen(slice.index);
        try self.gen(slice.rhs);
        try self.emit(.make_slice);
    }

    /// Creates a `ByteCode` object from the current instructions
    /// NOTE: This makes the instructions list on `self` invalid
    pub fn final(self: *Instructions) ByteCode {
        return .{
            .instructions = self.list.toOwnedSlice(self.gpa),
            .allocator = self.gpa,
        };
    }
};

/// Instruction generated by the compiler
/// Each instruction is encoded using little endian
pub const Instruction = union(Type) {
    op: Opcode,
    ptr: struct {
        op: Opcode,
        pos: u32,
    },
    integer: u64,
    string: []const u8,
    function: struct {
        name: []const u8,
        locals: u32,
        arg_len: u8,
        entry: u32,
    },

    const Type = enum { op, ptr, integer, string, function };

    /// Returns the Opcode of the Instruction
    pub fn getOp(self: Instruction) Opcode {
        return switch (self) {
            .op => self.op,
            .ptr => |ptr| ptr.op,
            .integer => .load_integer,
            .string => .load_string,
            .function => .load_func,
        };
    }

    /// Generates a single opcode
    pub fn gen(op: Opcode) Instruction {
        return .{ .op = op };
    }

    /// Generates a `ptr` instruction
    pub fn genPtr(op: Opcode, ptr: u32) Instruction {
        return .{ .ptr = .{ .op = op, .pos = ptr } };
    }

    /// Generates an `integer` instruction
    pub fn genInteger(value: u64) Instruction {
        return .{ .integer = value };
    }

    /// Generates a `string` instruction
    pub fn genString(value: []const u8) Instruction {
        return .{ .string = value };
    }

    /// Generates a `function` Instruction
    pub fn genFunction(name: []const u8, locals: usize, arg_len: usize, entry_point: u32) Instruction {
        return .{
            .function = .{
                .name = name,
                .locals = @intCast(u32, locals),
                .arg_len = @intCast(u8, arg_len),
                .entry = entry_point,
            },
        };
    }
};

/// Bytecode contains the list of instructions
pub const ByteCode = struct {
    instructions: []const Instruction,
    allocator: *std.mem.Allocator,

    /// Encodes instructions to bytecode
    /// memory has to be freed by the caller
    pub fn encode(self: ByteCode) ![]const u8 {
        return Encoder.encode(self.allocator, self.instructions);
    }

    /// Encodes the instructions and emits to a writer stream
    pub fn encodeToStream(self: ByteCode, writer: anytype) !void {
        return Encoder.writeToStream(writer, self.instructions);
    }

    /// Decodes the instructions from a stream and creates a new `ByteCode` struct
    pub fn decodeFromStream(gpa: *std.mem.Allocator, reader: anytype) !ByteCode {
        return ByteCode{
            .instructions = try Decoder.decode(reader, gpa),
            .allocator = gpa,
        };
    }

    /// Frees all memory generated by the compiler
    pub fn deinit(self: *ByteCode) void {
        for (self.instructions) |i| {
            if (i == .string) self.allocator.free(i.string);
            if (i == .function) self.allocator.free(i.function.name);
        }
        self.allocator.free(self.instructions);
        self.* = undefined;
    }

    /// Dumps human-readable bytecode representation to the given `writer` interface
    pub fn dump(self: ByteCode, writer: anytype) @TypeOf(writer).Error!void {
        for (self.instructions) |inst| {
            switch (inst) {
                .op => |op| try writer.print("{}\n", .{op}),
                .ptr => |ptr| try writer.print("{} {}\n", .{ ptr.op, ptr.pos }),
                .integer => |int| try writer.print("{} {d}\n", .{ inst.getOp(), int }),
                .string => |string| try writer.print("{} {s}\n", .{ inst.getOp(), string }),
                .function => |func| try writer.print("{} {s} {d} {d} {d}\n", .{
                    inst.getOp(),
                    func.name,
                    func.locals,
                    func.arg_len,
                    func.entry,
                }),
            }
        }
    }
};

/// Byte code writer that encodes by little endian.
pub const Encoder = struct {
    /// Encodes the instructions and returns the encoded bytecode for in-memory usage
    /// Memory is owned by the caller
    pub fn encode(allocator: *std.mem.Allocator, instructions: []const Instruction) ![]const u8 {
        var code = std.ArrayList(u8).init(allocator);
        for (instructions) |inst| {
            try emitInstruction(inst, code.writer());
        }
        return code.toOwnedSlice();
    }

    /// Encodes the instructions and writes it to the input stream
    pub fn writeToStream(writer: anytype, instructions: []const Instruction) @TypeOf(writer).Error!void {
        for (instructions) |inst| {
            try emitInstruction(inst, writer);
        }
    }

    fn emitInstruction(inst: Instruction, writer: anytype) @TypeOf(writer).Error!void {
        switch (inst) {
            .op => |op| try emitOp(writer, op),
            .ptr => |ptr| try emitPtr(writer, ptr.op, ptr.pos),
            .integer => |int| try emitInteger(writer, int),
            .string => |string| try emitString(writer, string),
            .function => |func| try emitFunc(writer, func.name, .{
                .locals = func.locals,
                .arg_len = func.arg_len,
                .entry = func.entry,
            }),
        }
    }

    /// Emits a single opcode.
    fn emitOp(writer: anytype, op: Opcode) @TypeOf(writer).Error!void {
        return writer.writeIntLittle(u8, @enumToInt(op));
    }

    /// Emits an opcode and a ptr it points to. This could be a constant, array length, etc
    fn emitPtr(writer: anytype, op: Opcode, ptr: u32) @TypeOf(writer).Error!void {
        try emitOp(writer, op);
        return writer.writeIntLittle(u32, ptr);
    }

    /// Emits a load_integer opcode followed by the bytes representing the integer's value
    fn emitInteger(writer: anytype, value: u64) @TypeOf(writer).Error!void {
        try emitOp(writer, .load_integer);
        return writer.writeIntLittle(u64, value);
    }

    /// Emits a load_string opcode followed the the length of the string encoded as u16.
    /// Finalized by writing the value of the string
    /// The max length of the string is 65536.
    fn emitString(writer: anytype, value: []const u8) @TypeOf(writer).Error!void {
        try emitOp(writer, .load_string);
        try writer.writeIntLittle(u16, @intCast(u16, value.len));
        return writer.writeAll(value);
    }

    /// Emits a `load_func` opcode where the struct is encoded as a byte slice
    fn emitFunc(
        writer: anytype,
        name: []const u8,
        func: struct {
            locals: u32,
            arg_len: u8,
            entry: u32,
        },
    ) @TypeOf(writer).Error!void {
        try emitOp(writer, .load_func);
        const len: u8 = @intCast(u8, name.len);
        try writer.writeIntLittle(u8, len);
        try writer.writeAll(name);
        try writer.writeIntLittle(u32, func.locals);
        try writer.writeIntLittle(u8, func.arg_len);
        try writer.writeIntLittle(u32, func.entry);
    }
};

/// Decoder for Luf's bytecode
pub const Decoder = struct {
    /// Decodes a stream into a list of `Instruction`
    pub fn decode(reader: anytype, allocator: *std.mem.Allocator) ![]Instruction {
        var instructions = std.ArrayList(Instruction).init(allocator);
        errdefer {
            for (instructions.items) |i| {
                if (i == .string) allocator.free(i.string);
                if (i == .function) allocator.free(i.function.name);
            }
            instructions.deinit();
        }

        var decoder = Decoder{};

        while (true) {
            const byte = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };

            const op = @intToEnum(Opcode, byte);
            const inst = try instructions.addOne();
            switch (op) {
                .load_func => try decoder.loadFunc(reader, inst, allocator),
                .load_string => try decoder.loadString(reader, inst, allocator),
                .load_integer => try decoder.loadInt(reader, inst),
                .load_global,
                .bind_global,
                .load_local,
                .bind_local,
                .assign_global,
                .assign_local,
                .jump_false,
                .jump,
                .make_array,
                .make_map,
                .make_enum,
                .call,
                => try decoder.loadPtr(reader, inst, op),
                else => inst.* = .{ .op = op },
            }
        }

        return instructions.toOwnedSlice();
    }

    /// Loads the current opcode into a fuction instruction
    fn loadFunc(self: *Decoder, reader: anytype, inst: *Instruction, allocator: *std.mem.Allocator) !void {
        const name_length = try reader.readIntLittle(u8);
        const name = if (name_length > 0) blk: {
            const string = try allocator.alloc(u8, name_length);
            errdefer allocator.free(string);

            if ((try reader.readAll(string)) < name_length)
                return error.InvalidBytecode;
            break :blk string;
        } else "";

        inst.* = .{
            .function = .{
                .name = name,
                .locals = try reader.readIntLittle(u32),
                .arg_len = try reader.readIntLittle(u8),
                .entry = try reader.readIntLittle(u32),
            },
        };
    }

    /// Loads a string instruction
    fn loadString(self: *Decoder, reader: anytype, inst: *Instruction, allocator: *std.mem.Allocator) !void {
        const string_length = try reader.readIntLittle(u16);
        const string = try allocator.alloc(u8, string_length);
        errdefer allocator.free(string);

        if ((try reader.readAll(string)) < string_length)
            return error.InvalidBytecode;

        inst.* = .{ .string = string };
    }

    /// Loads a 64 bit unsigned integer `Instruction`, it is up to the VM
    /// to bitcast it to a signed integer
    fn loadInt(self: *Decoder, reader: anytype, inst: *Instruction) !void {
        const int = try reader.readIntLittle(u64);
        inst.* = .{ .integer = int };
    }

    /// Loads an instruction that contains the opcode as well as the ptr towards an index
    fn loadPtr(
        self: *Decoder,
        reader: anytype,
        inst: *Instruction,
        op: Opcode,
    ) !void {
        const ptr = try reader.readIntLittle(u32);
        inst.* = .{ .ptr = .{ .op = op, .pos = ptr } };
    }
};

test "Encoding and decoding of instructions" {
    const allocator = testing.allocator;
    var buffer: [34]u8 = undefined;
    const instructions = &[_]Instruction{
        .{ .op = .load_false },
        .{ .string = "Hi" },
        .{ .integer = 5 },
        .{ .ptr = .{ .op = .jump, .pos = 5 } },
        .{ .function = .{ .name = "add", .locals = 2, .arg_len = 2, .entry = 1 } },
    };
    var stream = std.io.fixedBufferStream(&buffer);
    const code = try Encoder.encode(allocator, instructions);
    defer allocator.free(code);
    try Encoder.writeToStream(stream.writer(), instructions);

    const load_false = "\x05";
    const load_string = "\x01\x02\x00Hi";
    const load_int = "\x00\x05\x00\x00\x00\x00\x00\x00\x00";
    const load_ptr = "\x0F\x05\x00\x00\x00";
    const load_fn = "\x02\x03add\x02\x00\x00\x00\x02\x01\x00\x00\x00";
    try testing.expectEqualSlices(u8, load_false ++ load_string ++ load_int ++ load_ptr ++ load_fn, code);
    try testing.expectEqualSlices(u8, load_false ++ load_string ++ load_int ++ load_ptr ++ load_fn, stream.getWritten());

    stream.reset();
    const decoded = try Decoder.decode(stream.reader(), allocator);
    defer allocator.free(decoded);

    try testing.expectEqual(instructions.len, decoded.len);

    for (instructions) |inst, i| {
        switch (inst) {
            .op => try testing.expectEqual(inst.op, decoded[i].op),
            .ptr => try testing.expectEqual(inst.ptr.pos, decoded[i].ptr.pos),
            .string => try testing.expectEqualStrings(inst.string, decoded[i].string),
            .integer => try testing.expectEqual(inst.integer, decoded[i].integer),
            .function => |func| {
                try testing.expectEqualStrings(func.name, decoded[i].function.name);
                try testing.expectEqual(func.locals, decoded[i].function.locals);
                try testing.expectEqual(func.arg_len, decoded[i].function.arg_len);
                try testing.expectEqual(func.entry, decoded[i].function.entry);
            },
        }
    }

    for (decoded) |inst| {
        if (inst == .string) allocator.free(inst.string);
        if (inst == .function) allocator.free(inst.function.name);
    }
}

fn testInput(input: []const u8, expected: []const Opcode) !void {
    var alloc = testing.allocator;

    var err = @import("error.zig").Errors.init(alloc);
    defer err.deinit();

    var cu = try @import("compiler.zig").compile(alloc, input, &err);
    defer cu.deinit();

    var result = try Instructions.fromCu(alloc, cu);
    defer result.deinit();

    for (expected) |exp, i| {
        try testing.expectEqual(exp, result.instructions[i].getOp());
    }
}

test "IR to Bytecode - Arithmetic" {
    const test_cases = .{
        .{
            .input = "1 + 2",
            .opcodes = &[_]Opcode{ .load_integer, .load_integer, .add, .pop },
        },
        .{
            .input = "3 - 1",
            .opcodes = &[_]Opcode{ .load_integer, .load_integer, .sub, .pop },
        },
        .{
            .input = "1 * 2",
            .opcodes = &[_]Opcode{ .load_integer, .load_integer, .mul, .pop },
        },
        .{
            .input = "2 / 2",
            .opcodes = &[_]Opcode{ .load_integer, .load_integer, .div, .pop },
        },
        .{
            .input = "true",
            .opcodes = &[_]Opcode{ .load_true, .pop },
        },
        .{
            .input = "1 > 2",
            .opcodes = &[_]Opcode{ .load_integer, .load_integer, .greater_than, .pop },
        },
        .{
            .input = "1 < 2",
            .opcodes = &[_]Opcode{ .load_integer, .load_integer, .less_than, .pop },
        },
        .{
            .input = "1 == 2",
            .opcodes = &[_]Opcode{ .load_integer, .load_integer, .equal, .pop },
        },
        .{
            .input = "1 != 2",
            .opcodes = &[_]Opcode{ .load_integer, .load_integer, .not_equal, .pop },
        },
        .{
            .input = "true == false",
            .opcodes = &[_]Opcode{ .load_true, .load_false, .equal, .pop },
        },
        .{
            .input = "-1",
            .opcodes = &[_]Opcode{ .load_integer, .minus, .pop },
        },
        .{
            .input = "!true",
            .opcodes = &[_]Opcode{ .load_true, .not, .pop },
        },
    };

    inline for (test_cases) |case| {
        try testInput(case.input, case.opcodes);
    }
}

test "IR to Bytecode - Non control flow" {
    const test_cases = .{
        .{
            .input = "const x = 5",
            .opcodes = &[_]Opcode{ .load_integer, .bind_global },
        },
        .{
            .input = "const x = \"foo\"",
            .opcodes = &[_]Opcode{ .load_string, .bind_global },
        },
        .{
            .input = "const x = []int{1, 2, 3}",
            .opcodes = &[_]Opcode{ .load_integer, .load_integer, .load_integer, .make_array, .bind_global },
        },
        .{
            .input = "const x = []int:int{1: 2, 2: 1, 5: 6}",
            .opcodes = &[_]Opcode{
                .load_integer,
                .load_integer,
                .load_integer,
                .load_integer,
                .load_integer,
                .load_integer,
                .make_map,
                .bind_global,
            },
        },
        .{
            .input = "const x = 1..5",
            .opcodes = &[_]Opcode{
                .load_integer,
                .load_integer,
                .make_range,
                .bind_global,
            },
        },
        .{
            .input = "const x = enum{first_value, second_value, third_value}",
            .opcodes = &[_]Opcode{
                .load_string,
                .load_string,
                .load_string,
                .make_enum,
                .bind_global,
            },
        },
        .{
            .input = "mut x = 5 x = 10",
            .opcodes = &[_]Opcode{
                .load_integer,
                .bind_global,
                .load_integer,
                .assign_global,
                .pop,
            },
        },
    };

    inline for (test_cases) |case| {
        try testInput(case.input, case.opcodes);
    }
}

test "IR to Bytecode - Control flow" {
    const test_cases = .{
        .{
            .input = "if true { 5 } else { 7 } 10",
            .opcodes = &[_]Opcode{
                .load_true,
                .jump_false,
                .load_integer,
                .jump,
                .load_integer,
                .pop,
                .load_integer,
                .pop,
            },
        },
        .{
            .input = "fn() void { 1 + 2 }",
            .opcodes = &[_]Opcode{
                .jump,
                .load_integer,
                .load_integer,
                .add,
                .@"return",
                .load_func,
                .pop,
            },
        },
        .{
            .input = "const x = fn() void { 1 } x()",
            .opcodes = &[_]Opcode{
                .jump,
                .load_integer,
                .@"return",
                .load_func,
                .bind_global,
                .load_global,
                .call,
                .pop,
            },
        },
        .{
            .input = "const func = fn(x: int) int { return x } func(5)",
            .opcodes = &[_]Opcode{
                .jump,
                .load_local,
                .return_value,
                .load_func,
                .bind_global,
                .load_integer,
                .load_global,
                .call,
                .pop,
            },
        },
        .{
            .input = "mut i = 0 while (i > 10) { i = 10 }",
            .opcodes = &[_]Opcode{
                .load_integer,
                .bind_global,
                .load_global,
                .load_integer,
                .greater_than,
                .jump_false,
                .load_integer,
                .assign_global,
                .pop,
                .jump,
            },
        },
        .{
            .input = "switch(5){4: nil, 5: nil}",
            .opcodes = &[_]Opcode{
                .load_integer,
                .load_integer,
                .match,
                .jump_false,
                .load_nil,
                .pop,
                .load_integer,
                .match,
                .jump_false,
                .load_nil,
                .pop,
                .pop,
            },
        },
        .{
            .input = "while true {break continue}",
            .opcodes = &[_]Opcode{
                .load_true,
                .jump_false,
                .jump,
                .jump,
                .load_void,
                .pop,
                .jump,
            },
        },
        .{
            .input = "for x: 0..1 {}",
            .opcodes = &[_]Opcode{
                .load_integer,
                .load_integer,
                .make_range,
                .make_iter,
                .iter_next,
                .jump_false,
                .assign_local,
                .pop,
                .jump,
            },
        },
        .{
            .input = "for x, i: 0..1 {}",
            .opcodes = &[_]Opcode{
                .load_integer,
                .load_integer,
                .make_range,
                .make_iter,
                .iter_next,
                .jump_false,
                .assign_local,
                .pop,
                .assign_local,
                .pop,
                .jump,
            },
        },
        .{
            .input = "const list = []int{0,1} const slice = list[0:1]",
            .opcodes = &[_]Opcode{
                .load_integer,
                .load_integer,
                .make_array,
                .bind_global,
                .load_global,
                .load_integer,
                .load_integer,
                .make_slice,
                .bind_global,
            },
        },
    };

    inline for (test_cases) |case| {
        try testInput(case.input, case.opcodes);
    }
}
