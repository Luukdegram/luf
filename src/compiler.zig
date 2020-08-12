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

    // load builtins
    try root_scope.symbols.ensureCapacity(Value.builtin_keys.len);
    for (Value.builtin_keys) |key, i| {
        root_scope.symbols.putAssumeCapacity(key, .{
            .scope = .builtin,
            .name = key,
            .mutable = false,
            .index = @intCast(u16, i),
        });
    }

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
        /// The scope the symbol belongs to
        scope: Scope.Id,
    };

    /// Hashmap of `Symbol` where the key is the Symbol's name
    const SymbolTable = std.StringHashMap(Symbol);

    /// Scope of the current state (function, root, etc)
    const Scope = struct {
        symbols: SymbolTable,
        id: Id,
        parent: ?*Scope = null,
        allocator: *Allocator,

        const Id = enum {
            root,
            function,
            builtin,
        };

        /// Creates a new `Scope` from the current Scope.
        /// The new Scope will have its parent set to the current Scope
        fn fork(self: *Scope, id: Scope.Id) !*Scope {
            const new_scope = try self.allocator.create(Scope);
            new_scope.* = .{
                .symbols = SymbolTable.init(self.allocator),
                .id = id,
                .parent = self,
                .allocator = self.allocator,
            };
            return new_scope;
        }

        /// Defines a new symbol and saves it in the symbol table
        fn define(self: *Scope, name: []const u8, mutable: bool) Error!Symbol {
            if (self.resolve(name)) |s| return s;
            const index = if (self.id == .root)
                self.symbols.items().len - Value.builtin_keys.len
            else
                self.symbols.items().len;

            const symbol = Symbol{
                .name = name,
                .mutable = mutable,
                .index = @intCast(u16, index),
                .scope = self.id,
            };
            try self.symbols.put(name, symbol);
            return symbol;
        }

        /// Retrieves a `Symbol` from the Scopes symbol table, returns null if not found
        fn resolve(self: *const Scope, name: []const u8) ?Symbol {
            return self.symbols.get(name);
        }

        /// Cleans up the memory of the `Scope`
        fn deinit(self: *Scope) void {
            self.symbols.deinit();
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
        return @intCast(u16, self.constants.items.len - 1);
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

    /// Returns true if the last emitted instruction was given opcode
    fn lastInstIs(self: *Compiler, op: bytecode.Opcode) bool {
        return self.last_inst.op == op;
    }

    /// Removes the last instruction
    fn removeLastInst(self: *Compiler) void {
        _ = self.instructions.popOrNull();
        self.last_inst = self.prev_inst;
    }

    /// Sets the current `Scope` to its parent's scope and cleans up the closing scope's memory
    fn escapeScope(self: *Compiler) void {
        if (self.scope.id == .root) return; // can't escape the root scope
        if (self.scope.parent) |parent| {
            const old = self.scope;
            self.scope = parent;
            old.deinit();
        }
    }

    /// Creates a new Scope with the given Id, then sets the new scope as the current
    fn createScope(self: *Compiler, id: Scope.Id) !void {
        const fork = try self.scope.fork(id);
        self.scope = fork;
    }

    /// Attempts to resolve a symbol from the symbol table
    /// If not found, will attempt to resolve it from a parent scope
    fn resolveSymbol(self: *Compiler, scope: *const Scope, name: []const u8) ?Symbol {
        return if (scope.resolve(name)) |symbol|
            symbol
        else if (scope.id != .root and scope.parent != null)
            self.resolveSymbol(scope.parent.?, name)
        else
            null;
    }

    /// Compiles the given node into Instructions
    fn compile(self: *Compiler, node: ast.Node) Error!void {
        switch (node) {
            .expression => |exp| {
                try self.compile(exp.value);

                if (!self.lastInstIs(.noop)) {
                    _ = try self.emit(.pop);
                }
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
                    else => unreachable,
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
                if (self.lastInstIs(.pop)) self.removeLastInst();

                // Add a jump to the stack and return its position
                const jump_pos = try self.emit(.jump);

                // save current instructions position
                const cur_pos = self.instructions.items.len;

                // Point the position of the false jump to the current position
                self.instructions.items[false_pos].ptr = @intCast(u16, cur_pos);

                if (if_exp.false_pong) |pong| {
                    try self.compile(pong);

                    if (self.lastInstIs(.pop)) self.removeLastInst();
                } else {
                    _ = try self.emit(.load_nil);
                }

                // set the true jump to the current stack position
                const len = self.instructions.items.len;
                self.instructions.items[jump_pos].ptr = @intCast(u16, len);
            },
            .declaration => |decl| {
                const symbol = try self.scope.define(decl.name.identifier.value, decl.mutable);

                try self.compile(decl.value);

                const opcode: bytecode.Opcode = if (symbol.scope == .root)
                    .bind_global
                else
                    .bind_local;

                _ = try self.emitOp(opcode, symbol.index);
            },
            .identifier => |id| if (self.resolveSymbol(self.scope, id.value)) |symbol| {
                const opcode: bytecode.Opcode = switch (symbol.scope) {
                    .root => .load_global,
                    .function => .load_local,
                    .builtin => .load_builtin,
                };
                _ = try self.emitOp(opcode, symbol.index);
            } else return Error.CompilerError,
            .string_lit => |string| {
                const val = Value{ .string = try self.scope.allocator.dupe(u8, string.value) };
                _ = try self.emitOp(.load_const, try self.addConstant(val));
            },
            .array => |array| {
                for (array.value) |element| {
                    try self.compile(element);
                }
                _ = try self.emitOp(.make_array, @intCast(u16, array.value.len));
            },
            .map => |map| {
                for (map.value) |pair| try self.compile(pair);
                _ = try self.emitOp(.make_map, @intCast(u16, map.value.len * 2));
            },
            .map_pair => |pair| {
                try self.compile(pair.key);
                try self.compile(pair.value);
            },
            .index => |index| {
                try self.compile(index.left);
                try self.compile(index.index);
                _ = try self.emit(.index);
            },
            .func_lit => |function| {
                const jump_pos = try self.emit(.jump);
                try self.createScope(.function);

                for (function.params) |param| {
                    // function arguments are not mutable by default
                    _ = try self.scope.define(param.identifier.value, false);
                }

                const offset = self.instructions.items.len;

                try self.compile(function.body);

                // if the last instruction is a pop rather than some value
                // replace it with a normal return statement
                if (self.lastInstIs(.pop)) {
                    const last: *bytecode.Instruction = &self.instructions.items[self.last_inst.pos];
                    last.op = .return_value;
                    self.last_inst.op = last.op;
                }

                // if no return_value found, emit a regular return instruction
                if (!self.lastInstIs(.return_value)) _ = try self.emit(._return);

                self.escapeScope();

                const last_pos = try self.emitOp(.load_const, try self.addConstant(.{
                    .function = .{
                        .offset = offset,
                        .arg_len = function.params.len,
                    },
                }));

                self.instructions.items[jump_pos].ptr = @intCast(u16, last_pos);
            },
            .call_expression => |call| {
                try self.compile(call.function);
                for (call.arguments) |arg| {
                    try self.compile(arg);
                }
                _ = try self.emitOp(.call, @intCast(u16, call.arguments.len));
            },
            ._return => |ret| {
                try self.compile(ret.value);
                _ = try self.emit(.return_value);
            },
            .while_loop => |loop| {
                // beginning of while
                const start_jump = self.instructions.items.len;
                try self.compile(loop.condition);

                // jump position if condition equals false
                const false_jump = try self.emit(.jump_false);

                try self.compile(loop.block);

                _ = try self.emitOp(.jump, @intCast(u16, start_jump));

                const end = try self.emit(.noop);

                // jump to end
                self.instructions.items[false_jump].ptr = @intCast(u16, end);
            },
            .assignment => |asg| {
                if (asg.name != .identifier) return Error.CompilerError;
                const symbol = self.resolveSymbol(self.scope, asg.name.identifier.value) orelse return Error.CompilerError;

                if (!symbol.mutable) return Error.CompilerError;

                try self.compile(asg.value);

                if (symbol.scope == .root)
                    _ = try self.emitOp(.assign_global, symbol.index)
                else
                    _ = try self.emitOp(.assign_local, symbol.index);
            }, //else => return Error.CompilerError,
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
        .{
            .input = "const x = [1, 2, 3]",
            .consts = &[_]i64{ 1, 2, 3 },
            .opcodes = &[_]bytecode.Opcode{ .load_const, .load_const, .load_const, .make_array, .bind_global },
        },
        .{
            .input = "const x = {1: 2, 2: 1, 5: 6}",
            .consts = &[_]i64{ 1, 2, 2, 1, 5, 6 },
            .opcodes = &[_]bytecode.Opcode{
                .load_const,
                .load_const,
                .load_const,
                .load_const,
                .load_const,
                .load_const,
                .make_map,
                .bind_global,
            },
        },
        .{
            .input = "[1][0]",
            .consts = &[_]i64{ 1, 0 },
            .opcodes = &[_]bytecode.Opcode{
                .load_const,
                .make_array,
                .load_const,
                .index,
                .pop,
            },
        },
        .{
            .input = "{1: 10}[0]",
            .consts = &[_]i64{ 1, 10, 0 },
            .opcodes = &[_]bytecode.Opcode{
                .load_const,
                .load_const,
                .make_map,
                .load_const,
                .index,
                .pop,
            },
        },
        .{
            .input = "fn(){ 1 + 2 }",
            .consts = &[_]Value{ Value{ .integer = 1 }, Value{ .integer = 2 }, Value{ .function = .{ .offset = 1, .arg_len = 0 } } },
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                .load_const,
                .load_const,
                .add,
                .return_value,
                .load_const,
                .pop,
            },
        },
        .{
            .input = "fn(){ }",
            .consts = &[_]Value{Value{ .function = .{ .offset = 1, .arg_len = 0 } }},
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                ._return,
                .load_const,
                .pop,
            },
        },
        .{
            .input = "fn(){ 1 }()",
            .consts = &[_]Value{ Value{ .integer = 1 }, Value{ .function = .{ .offset = 1, .arg_len = 0 } } },
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                .load_const,
                .return_value,
                .load_const,
                .call,
                .pop,
            },
        },
        .{
            .input = "const x = fn(){ 1 } x()",
            .consts = &[_]Value{ Value{ .integer = 1 }, Value{ .function = .{ .offset = 1, .arg_len = 0 } } },
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                .load_const,
                .return_value,
                .load_const,
                .bind_global,
                .load_global,
                .call,
                .pop,
            },
        },
        .{
            .input = "const x = 5 fn(){ x }",
            .consts = &[_]Value{ Value{ .integer = 5 }, Value{ .function = .{ .offset = 3, .arg_len = 0 } } },
            .opcodes = &[_]bytecode.Opcode{
                .load_const,
                .bind_global,
                .jump,
                .load_global,
                .return_value,
                .load_const,
                .pop,
            },
        },
        .{
            .input = "fn(){ const x = 5 x }",
            .consts = &[_]Value{ Value{ .integer = 5 }, Value{ .function = .{ .offset = 1, .arg_len = 0 } } },
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                .load_const,
                .bind_local,
                .load_local,
                .return_value,
                .load_const,
                .pop,
            },
        },
        .{
            .input = "const func = fn(x){ x } func(5)",
            .consts = &[_]Value{ Value{ .function = .{ .offset = 1, .arg_len = 1 } }, Value{ .integer = 5 } },
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                .load_local,
                .return_value,
                .load_const,
                .bind_global,
                .load_global,
                .load_const,
                .call,
                .pop,
            },
        },
        .{
            .input = "const x = \"string\".len",
            .consts = &[_][]const u8{"string"},
            .opcodes = &[_]bytecode.Opcode{
                .load_const,
                .load_builtin,
                .index,
                .bind_global,
            },
        },
        .{
            .input = "while (true) { 10 }",
            .consts = &[_]i64{10},
            .opcodes = &[_]bytecode.Opcode{
                .load_true,
                .jump_false,
                .load_const,
                .pop,
                .jump,
                .noop,
            },
        },
        .{
            .input = "mut x = 5 x = 6",
            .consts = &[_]i64{ 5, 6 },
            .opcodes = &[_]bytecode.Opcode{
                .load_const,
                .bind_global,
                .load_const,
                .assign_global,
            },
        },
    };

    inline for (test_cases) |case| {
        const code = try compile(testing.allocator, case.input);
        defer code.deinit();

        testing.expectEqual(case.consts.len, code.constants.len);
        testing.expectEqual(case.opcodes.len, code.instructions.len);
        for (case.consts) |constant, i| {
            if (@TypeOf(constant) == i64)
                testing.expect(constant == code.constants[i].integer)
            else if (@TypeOf(constant) == []const u8)
                testing.expectEqualStrings(constant, code.constants[i].string)
            else {
                //expect a `Value`
                switch (constant) {
                    .integer => testing.expectEqual(constant.integer, code.constants[i].integer),
                    .function => testing.expectEqual(constant.function.offset, code.constants[i].function.offset),
                    else => {},
                }
            }
        }
        for (case.opcodes) |op, i| {
            //std.debug.print("Instr: {}\n", .{code.instructions[i]});
            testing.expectEqual(op, code.instructions[i].op);
        }
    }
}
