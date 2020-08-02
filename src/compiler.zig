const std = @import("std");
const bytecode = @import("bytecode.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const testing = std.testing;

//! Contains the Compiler internals.
//! This compiles all AST nodes generated by the parser and turned into `ByteCode` instructions
//! Those instructions are than used by the vm to be executed.
//! Potentially, we could provide a different back-end to support
//! different use cases.

/// Compiles the source code into Luf's bytecode
pub fn compile(
    allocator: *Allocator,
    source: []const u8,
) (parser.Parser.Error || Compiler.Error)!Compiler.ByteCode {
    const tree = try parser.parse(allocator, source);
    defer tree.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    var root_scope = Compiler.Scope{
        .symbols = Compiler.SymbolTable.init(allocator),
        .id = .root,
        .allocator = &arena.allocator,
    };
    defer root_scope.symbols.deinit();

    var compiler = Compiler{
        .instructions = bytecode.Instructions.init(allocator),
        .constants = Values.init(allocator),
        .allocator = allocator,
        .last_inst = undefined,
        .prev_inst = undefined,
        .scope = &root_scope,
    };

    for (tree.nodes) |node| {
        try compiler.compile(node);
    }

    return Compiler.ByteCode{
        .instructions = compiler.instructions.toOwnedSlice(),
        .constants = compiler.constants.toOwnedSlice(),
        .allocator = allocator,
        .state = arena.state,
    };
}

/// List of `Value`
const Values = std.ArrayList(Value);

/// Contains the memory and generated instructions
/// after calling Compile()
pub const Compiler = struct {
    instructions: bytecode.Instructions,
    constants: Values,
    allocator: *Allocator,

    last_inst: EmitInst,
    prev_inst: EmitInst,

    scope: *Scope,

    /// Symbols known by the compiler
    const Symbol = struct {
        name: []const u8,
        /// Not mutable by default
        mutable: bool = false,
        /// Index of symbol table
        index: u16 = 0,
    };

    /// Hashmap of `Symbol` where the key is the Symnbol's name
    const SymbolTable = std.StringHashMap(Symbol);

    /// Scope of the current state (function, root, etc)
    const Scope = struct {
        symbols: SymbolTable,
        id: enum {
            root,
            function,
        },
        parent: ?*Scope = null,
        allocator: *Allocator,

        /// Creates a new `Scope` from the current Scope.
        /// The new Scope will have its parent set to the current Scope
        fn fork(self: Scope, id: Scope.id) !*Scope {
            const fork = try self.allocator.create(Scope);
            fork.* = .{
                .symbols = SymbolTable.init(self.allocator),
                .id = id,
                .parent = &self,
                .allocator = self.allocator,
            };
            return fork;
        }

        /// Defines a new symbol and saves it in the symbol table
        fn define(self: *Scope, name: []const u8, mutable: bool) !Symbol {
            const symbol = Symbol{
                .name = name,
                .mutable = mutable,
                .index = @truncate(u16, self.symbols.items().len),
            };
            try self.symbols.put(name, symbol);
            return symbol;
        }

        /// Retrieves a `Symbol` from the Scopes symbol table, returns null if not found
        fn resolve(self: *Scope, name: []const u8) ?Symbol {
            return self.symbols.get(name);
        }
    };

    /// Instruction that has already been emitted.
    /// This is used to generate a new instruction if it needs to be reapplied
    /// after being emitted
    const EmitInst = struct {
        op: bytecode.Opcode,
        pos: usize,
    };

    /// Compiler errorset
    pub const Error = error{ CompilerError, OutOfMemory };

    /// Generated bytecode by the `Compiler`
    pub const ByteCode = struct {
        instructions: []const bytecode.Instruction,
        constants: []const Value,
        allocator: *Allocator,
        state: std.heap.ArenaAllocator.State,

        /// Frees all memory generated by the compiler
        pub fn deinit(self: ByteCode) void {
            self.allocator.free(self.instructions);
            self.allocator.free(self.constants);
            self.state.promote(self.allocator).deinit();
        }
    };

    /// Adds a new constant to the compiler and returns the length of the list
    fn addConstant(self: *Compiler, val: Value) !u16 {
        try self.constants.append(val);
        return @truncate(u16, self.constants.items.len - 1);
    }

    /// Appends a new instruction to the compiler and returns the current position
    fn emit(self: *Compiler, op: bytecode.Opcode) !usize {
        const pos = self.instructions.items.len;
        try self.instructions.append(bytecode.gen(op, null));
        self.saveEmitInst(op, pos);
        return pos;
    }

    /// Appends a new instruction and saves the operand and returns the current position
    fn emitOp(self: *Compiler, op: bytecode.Opcode, operand: u16) !usize {
        const pos = self.instructions.items.len;
        try self.instructions.append(bytecode.gen(op, operand));
        self.saveEmitInst(op, pos);
        return pos;
    }

    /// Sets the previous and last instruction that were emitted
    fn saveEmitInst(self: *Compiler, op: bytecode.Opcode, pos: usize) void {
        self.prev_inst = self.last_inst;
        self.last_inst = EmitInst{ .op = op, .pos = pos };
    }

    /// Returns true if the last emitted instruction was `.pop`
    fn lastIsPop(self: *Compiler) bool {
        return self.last_inst.op == .pop;
    }

    /// Removes the last instruction
    fn removeLastInst(self: *Compiler) void {
        _ = self.instructions.popOrNull();
        self.last_inst = self.prev_inst;
    }

    /// Compiles the given node into Instructions
    fn compile(self: *Compiler, node: ast.Node) Error!void {
        switch (node) {
            .expression => |exp| {
                try self.compile(exp.value);
                _ = try self.emit(.pop);
            },
            .block_statement => |block| for (block.nodes) |bnode| try self.compile(bnode),
            .infix => |inf| {
                try self.compile(inf.left);
                try self.compile(inf.right);
                switch (inf.operator) {
                    .add => _ = try self.emit(.add),
                    .multiply => _ = try self.emit(.mul),
                    .sub => _ = try self.emit(.sub),
                    .divide => _ = try self.emit(.div),
                    .less_than => _ = try self.emit(.less_than),
                    .greater_than => _ = try self.emit(.greater_than),
                    .equal => _ = try self.emit(.equal),
                    .not_equal => _ = try self.emit(.not_equal),
                    .assign => return Error.CompilerError,
                }
            },
            .prefix => |pfx| {
                try self.compile(pfx.right);
                switch (pfx.operator) {
                    .minus => _ = try self.emit(.minus),
                    .bang => _ = try self.emit(.bang),
                }
            },
            .boolean => |boolean| _ = try self.emit(if (boolean.value) .load_true else .load_false),
            .int_lit => |int| {
                const val: Value = .{ .integer = @bitCast(i64, int.value) };
                _ = try self.emitOp(.load_const, try self.addConstant(val));
            },
            .if_expression => |if_exp| {
                try self.compile(if_exp.condition);
                // false is last so put it bottom of the added stack
                const false_pos = try self.emit(.jump_false);

                // compile the true pong and place the emitted opcodes on the stack
                try self.compile(if_exp.true_pong);

                // remove potential pop opcode as we continue the expression
                if (self.lastIsPop()) self.removeLastInst();

                // Add a jump to the stack and return its position
                const jump_pos = try self.emit(.jump);

                // save current instructions position
                const cur_pos = self.instructions.items.len;

                // Point the position of the false jump to the current position
                self.instructions.items[false_pos].ptr = @truncate(u16, cur_pos);

                if (if_exp.false_pong) |pong| {
                    try self.compile(pong);

                    if (self.lastIsPop()) self.removeLastInst();
                } else {
                    _ = try self.emit(.load_nil);
                }

                // set the true jump to the current stack position
                const len = self.instructions.items.len;
                self.instructions.items[jump_pos].ptr = @truncate(u16, len);
            },
            .declaration => |decl| {
                try self.compile(decl.value);
                const symbol = try self.scope.define(decl.name.identifier.value, decl.mutable);
                _ = try self.emitOp(.bind_global, symbol.index);
            },
            .identifier => |id| if (self.scope.resolve(id.value)) |symbol| {
                _ = try self.emitOp(.load_global, symbol.index);
            } else return Error.CompilerError,
            .string_lit => |string| {
                const val = Value{ .string = try self.scope.allocator.dupe(u8, string.value) };
                _ = try self.emitOp(.load_const, try self.addConstant(val));
            },

            else => return Error.CompilerError,
        }
    }
};

test "Compile AST to bytecode" {
    const test_cases = .{
        .{
            .input = "1 + 2",
            .consts = &[_]i64{ 1, 2 },
            .opcodes = &[_]bytecode.Opcode{ .load_const, .load_const, .add, .pop },
        },
        .{
            .input = "3 - 1",
            .consts = &[_]i64{ 3, 1 },
            .opcodes = &[_]bytecode.Opcode{ .load_const, .load_const, .sub, .pop },
        },
        .{
            .input = "1 * 2",
            .consts = &[_]i64{ 1, 2 },
            .opcodes = &[_]bytecode.Opcode{ .load_const, .load_const, .mul, .pop },
        },
        .{
            .input = "2 / 2",
            .consts = &[_]i64{ 2, 2 },
            .opcodes = &[_]bytecode.Opcode{ .load_const, .load_const, .div, .pop },
        },
        .{
            .input = "true",
            .consts = &[_]i64{},
            .opcodes = &[_]bytecode.Opcode{ .load_true, .pop },
        },
        .{
            .input = "1 > 2",
            .consts = &[_]i64{ 1, 2 },
            .opcodes = &[_]bytecode.Opcode{ .load_const, .load_const, .greater_than, .pop },
        },
        .{
            .input = "1 < 2",
            .consts = &[_]i64{ 1, 2 },
            .opcodes = &[_]bytecode.Opcode{ .load_const, .load_const, .less_than, .pop },
        },
        .{
            .input = "1 == 2",
            .consts = &[_]i64{ 1, 2 },
            .opcodes = &[_]bytecode.Opcode{ .load_const, .load_const, .equal, .pop },
        },
        .{
            .input = "1 != 2",
            .consts = &[_]i64{ 1, 2 },
            .opcodes = &[_]bytecode.Opcode{ .load_const, .load_const, .not_equal, .pop },
        },
        .{
            .input = "true == false",
            .consts = &[_]i64{},
            .opcodes = &[_]bytecode.Opcode{ .load_true, .load_false, .equal, .pop },
        },
        .{
            .input = "-1",
            .consts = &[_]i64{1},
            .opcodes = &[_]bytecode.Opcode{ .load_const, .minus, .pop },
        },
        .{
            .input = "!true",
            .consts = &[_]i64{},
            .opcodes = &[_]bytecode.Opcode{ .load_true, .bang, .pop },
        },
        .{
            .input = "if (true) { 5 } 10",
            .consts = &[_]i64{ 5, 10 },
            .opcodes = &[_]bytecode.Opcode{
                .load_true,
                .jump_false,
                .load_const,
                .jump,
                .load_nil,
                .pop,
                .load_const,
                .pop,
            },
        },
        .{
            .input = "if (true) { 5 } else { 7 } 10",
            .consts = &[_]i64{ 5, 7, 10 },
            .opcodes = &[_]bytecode.Opcode{
                .load_true,
                .jump_false,
                .load_const,
                .jump,
                .load_const,
                .pop,
                .load_const,
                .pop,
            },
        },
        .{
            .input = "const x = \"foo\"",
            .consts = &[_][]const u8{"foo"},
            .opcodes = &[_]bytecode.Opcode{ .load_const, .bind_global },
        },
    };

    inline for (test_cases) |case| {
        const code = try compile(testing.allocator, case.input);
        defer code.deinit();

        testing.expect(case.consts.len == code.constants.len);
        testing.expect(case.opcodes.len == code.instructions.len);
        for (case.consts) |constant, i| {
            if (@TypeOf(constant) == i64)
                testing.expect(constant == code.constants[i].integer)
            else
                testing.expectEqualStrings(constant, code.constants[i].string);
        }
        for (case.opcodes) |op, i| {
            testing.expect(op == code.instructions[i].op);
        }
    }
}
