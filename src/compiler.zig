const std = @import("std");
const bytecode = @import("bytecode.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;
const testing = std.testing;
const errors = @import("error.zig");

//! Contains the Compiler internals.
//! This compiles all AST nodes generated by the parser and turned into `ByteCode` instructions
//! Those instructions are than used by the vm to be executed.
//! Potentially, we could provide a different back-end to support
//! different use cases.

/// Compiles the source code into Luf's bytecode
pub fn compile(
    allocator: *Allocator,
    source: []const u8,
    _errors: *errors.Errors,
) (parser.Parser.Error || Compiler.Error)!Compiler.ByteCode {
    const tree = try parser.parse(allocator, source, _errors);
    defer tree.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var root_scope = Compiler.Scope{
        .symbols = Compiler.SymbolTable.init(allocator),
        .id = .global,
        .allocator = allocator,
    };
    defer root_scope.symbols.deinit();

    var compiler = Compiler{
        .instructions = bytecode.Instructions.init(allocator),
        .constants = Values.init(allocator),
        .allocator = allocator,
        .last_inst = undefined,
        .prev_inst = undefined,
        .scope = &root_scope,
        .errors = _errors,
    };
    errdefer compiler.instructions.deinit();
    errdefer compiler.constants.deinit();

    for (tree.nodes) |node| {
        try compiler.compile(node);
    }

    return Compiler.ByteCode{
        .instructions = compiler.instructions.toOwnedSlice(),
        .constants = compiler.constants.toOwnedSlice(),
        .allocator = allocator,
        .state = arena.state,
        .symbols = try root_scope.symbols.clone(),
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
    errors: *errors.Errors,
    last_inst: EmitInst,
    prev_inst: EmitInst,

    scope: *Scope,

    /// Symbols known by the compiler
    pub const Symbol = struct {
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

    /// Scope of the current state (function, global, etc)
    const Scope = struct {
        /// Symbols that exist within the current scope
        symbols: SymbolTable,
        /// Type of the Scope. i.e. Root, function, etc
        id: Tag,
        /// If not a global scope, the scope will own a parent
        parent: ?*Scope = null,
        allocator: *Allocator,

        /// The type of the scope
        const Id = enum {
            global,
            function,
            loop,
        };

        /// Some `Scope` types can have a payload
        const Tag = union(Id) {
            global,
            function: void,
            loop: struct {
                start: u16,
                breaks: std.ArrayList(*bytecode.Instruction),
            },
        };

        /// Creates a new `Scope` from the current Scope.
        /// The new Scope will have its parent set to the current Scope
        fn fork(self: *Scope, id: Scope.Id) !*Scope {
            const new_scope = try self.allocator.create(Scope);
            new_scope.* = .{
                .symbols = SymbolTable.init(self.allocator),
                .id = switch (id) {
                    .global => .{ .global = {} },
                    .function => .{ .function = {} },
                    .loop => .{ .loop = .{ .start = 0, .breaks = std.ArrayList(*bytecode.Instruction).init(self.allocator) } },
                },
                .parent = self,
                .allocator = self.allocator,
            };
            return new_scope;
        }

        /// Defines a new symbol and saves it in the symbol table
        /// Returns an error if Symbol already exists
        fn define(self: *Scope, name: []const u8, mutable: bool) !?Symbol {
            if (self.resolve(name)) |_| return null;
            const index = self.symbols.items().len;

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
            if (self.id == .loop) self.id.loop.breaks.deinit();
            self.symbols.deinit();
            self.allocator.destroy(self);
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
        symbols: SymbolTable,

        /// Frees all memory generated by the compiler
        pub fn deinit(self: *ByteCode) void {
            self.allocator.free(self.instructions);
            for (self.constants) |val| {
                if (val.isType(.function)) {
                    self.allocator.free(val.function.instructions);
                }
                if (val.isType(._enum)) {
                    self.allocator.free(val._enum);
                }
                if (val.isType(.string)) {
                    self.allocator.free(val.string);
                }
            }
            self.allocator.free(self.constants);
            self.symbols.deinit();
            self.state.promote(self.allocator).deinit();
            self.* = undefined;
        }
    };

    /// Returns `Error.CompilerError` and appends an error message to the `errors` list.
    fn fail(self: *Compiler, msg: []const u8, index: usize) Error {
        try self.errors.add(msg, index, .err);
        return Error.CompilerError;
    }

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
    fn exitScope(self: *Compiler) void {
        if (self.scope.id == .global) return; // can't escape the global scope
        if (self.scope.parent) |parent| {
            const old = self.scope;
            self.scope = parent;
            old.deinit();
        }
    }

    /// Creates a new Scope with the given Id, then sets the new scope as the current
    fn createScope(self: *Compiler, id: Scope.Id) !void {
        self.scope = try self.scope.fork(id);
    }

    /// Attempts to resolve a symbol from the symbol table
    /// If not found, will attempt to resolve it from a parent scope
    fn resolveSymbol(self: *Compiler, scope: *const Scope, name: []const u8) ?Symbol {
        return if (scope.resolve(name)) |symbol|
            symbol
        else if (scope.id != .global and scope.parent != null)
            self.resolveSymbol(scope.parent.?, name)
        else
            null;
    }

    /// Compiles the given node into Instructions
    fn compile(self: *Compiler, node: ast.Node) Error!void {
        switch (node) {
            .expression => |exp| {
                try self.compile(exp.value);
                _ = try self.emit(.pop);
            },
            .block_statement => |block| {
                for (block.nodes) |bnode| try self.compile(bnode);

                if (self.lastInstIs(.pop))
                    self.removeLastInst()
                else if (!self.lastInstIs(.return_value))
                    _ = try self.emit(.load_void);
            },
            .infix => |inf| {
                try self.compile(inf.left);
                try self.compile(inf.right);
                _ = try self.emit(switch (inf.operator) {
                    .add => .add,
                    .multiply => .mul,
                    .sub => .sub,
                    .divide => .div,
                    .less_than => .less_than,
                    .greater_than => .greater_than,
                    .equal => .equal,
                    .not_equal => .not_equal,
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
            },
            .prefix => |pfx| {
                try self.compile(pfx.right);
                _ = try self.emit(switch (pfx.operator) {
                    .minus => .minus,
                    .bang => .not,
                    .bitwise_not => .bitwise_not,
                });
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
                    _ = try self.emit(.load_void);
                }

                // set the true jump to the current stack position
                const len = self.instructions.items.len;
                self.instructions.items[jump_pos].ptr = @intCast(u16, len);
            },
            .declaration => |decl| {
                const symbol = (try self.scope.define(decl.name.identifier.value, decl.mutable)) orelse return self.fail(
                    "Identifier has already been declared",
                    decl.token.start,
                );

                try self.compile(decl.value);

                const opcode: bytecode.Opcode = if (symbol.scope == .global)
                    .bind_global
                else
                    .bind_local;

                _ = try self.emitOp(opcode, symbol.index);
            },
            .identifier => |id| if (self.resolveSymbol(self.scope, id.value)) |symbol| {
                const opcode: bytecode.Opcode = switch (symbol.scope) {
                    .global => .load_global,
                    .function, .loop => .load_local,
                };
                _ = try self.emitOp(opcode, symbol.index);
            } else return self.fail("Identifier does not exist", id.token.start),
            .string_lit => |string| {
                const val = Value{ .string = try self.allocator.dupe(u8, string.value) };
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
                _ = try self.emit(.get_by_index);
            },
            .func_lit => |function| {
                const jump_pos = try self.emit(.jump);
                try self.createScope(.function);
                const inst_ptr = self.instructions.items.len;

                for (function.params) |param| {
                    // function arguments are not mutable by default
                    _ = (try self.scope.define(param.identifier.value, false)) orelse return self.fail(
                        "Identifier has already been declared",
                        function.token.start,
                    );
                }

                try self.compile(function.body);

                // if the last instruction is a pop rather than some value
                // replace it with a normal return statement
                if (self.lastInstIs(.pop)) {
                    const last: *bytecode.Instruction = &self.instructions.items[self.last_inst.pos];
                    last.op = .return_value;
                    self.last_inst.op = last.op;
                }

                // if no return_value found, emit a regular return instruction
                if (!self.lastInstIs(.return_value)) _ = try self.emit(.@"return");

                const locals = self.scope.symbols.items().len;
                self.exitScope();

                const last_pos = try self.emitOp(.load_const, try self.addConstant(.{
                    .function = .{
                        .arg_len = function.params.len,
                        .locals = locals,
                        .instructions = try self.allocator.dupe(
                            bytecode.Instruction,
                            self.instructions.items[inst_ptr..self.instructions.items.len],
                        ),
                    },
                }));

                self.instructions.items[jump_pos].ptr = @intCast(u16, last_pos);
            },
            .call_expression => |call| {
                // function is either builtin or defined on a type
                if (call.function == .index) {
                    for (call.arguments) |arg| {
                        try self.compile(arg);
                    }
                    try self.compile(call.function);
                } else {
                    try self.compile(call.function);
                    for (call.arguments) |arg| {
                        try self.compile(arg);
                    }
                    _ = try self.emitOp(.call, @intCast(u16, call.arguments.len));
                }
            },
            .@"return" => |ret| {
                try self.compile(ret.value);
                _ = try self.emit(.return_value);
            },
            .while_loop => |loop| {
                try self.createScope(.loop);

                // beginning of while
                self.scope.id.loop.start = @intCast(u16, self.instructions.items.len);

                try self.compile(loop.condition);

                // jump position if condition equals false
                const false_jump = try self.emit(.jump_false);

                try self.compile(loop.block);

                _ = try self.emit(.pop);

                _ = try self.emitOp(.jump, @intCast(u16, self.scope.id.loop.start));

                const end = try self.emit(.pop);
                // jump to end
                self.instructions.items[false_jump].ptr = @intCast(u16, end);

                for (self.scope.id.loop.breaks.items) |inst| {
                    inst.ptr = @intCast(u16, end);
                }
                self.exitScope();
            },
            .for_loop => |loop| {
                try self.createScope(.loop);

                try self.compile(loop.iter);

                _ = try self.emitOp(.make_iter, @as(u16, if (loop.index != null) 1 else 0));
                const start_jump = try self.emit(.iter_next);
                self.scope.id.loop.start = @intCast(u16, start_jump);

                // jump position if we reached the end of the iterator
                const end_jump = try self.emit(.jump_false);

                // parser already parses it as an identifier, no need to check here again
                const capture = (try self.scope.define(loop.capture.identifier.value, false)) orelse return self.fail(
                    "Capture identifier has already been declared",
                    loop.token.start,
                );
                if (capture.scope != .loop) return self.fail("Expected a loop scope", loop.token.start);
                _ = try self.emitOp(.assign_local, capture.index);

                // as above, parser ensures it's an identifier
                if (loop.index) |i| {
                    const symbol = (try self.scope.define(i.identifier.value, false)) orelse return self.fail(
                        "Index identifier has already been declared",
                        loop.token.start,
                    );
                    if (symbol.scope != .loop) return self.fail("Expected a loop scope", loop.token.start);
                    _ = try self.emitOp(.assign_local, symbol.index);
                }

                try self.compile(loop.block);

                // pop last value from block
                _ = try self.emit(.pop);

                // jump to start of loop to evaluate range
                _ = try self.emitOp(.jump, self.scope.id.loop.start);

                // pop capture and index from stack
                const end = try self.emit(.pop);
                if (loop.index) |_| {
                    _ = try self.emit(.pop);
                }

                for (self.scope.id.loop.breaks.items) |inst| {
                    inst.ptr = @intCast(u16, end);
                }

                // point the end jump to last op
                self.instructions.items[end_jump].ptr = @intCast(u16, end);

                self.exitScope();
            },
            .assignment => |asg| {
                if (asg.left == .identifier) {
                    const symbol = self.resolveSymbol(
                        self.scope,
                        asg.left.identifier.value,
                    ) orelse return self.fail("Identifier could not be found", asg.token.start);

                    if (!symbol.mutable) return self.fail("Identifier is constant", asg.token.start);

                    try self.compile(asg.right);

                    if (symbol.scope == .global)
                        _ = try self.emitOp(.assign_global, symbol.index)
                    else
                        _ = try self.emitOp(.assign_local, symbol.index);
                } else if (asg.left == .index) {
                    const index = asg.left.index;
                    try self.compile(index.left);
                    try self.compile(index.index);
                    try self.compile(asg.right);
                    _ = try self.emit(.set_by_index);
                } else {
                    return self.fail("Expected an identifier or index on left hand side", asg.token.start);
                }
            },
            .nil => _ = try self.emit(.load_nil),
            .import => |imp| {
                try self.compile(imp.value);
                _ = try self.emit(.load_module);
            },
            .@"break" => |br| {
                if (self.scope.id != .loop)
                    return self.fail(
                        "Breaks can only be used while inside a loop",
                        br.token.start,
                    );

                const pos = try self.emit(.jump);
                const jump = &self.instructions.items[pos];
                try self.scope.id.loop.breaks.append(jump);
            },
            .@"continue" => |cont| {
                if (self.scope.id != .loop)
                    return self.fail(
                        "Continue can only be used while inside a loop",
                        cont.token.start,
                    );

                _ = try self.emitOp(.jump, self.scope.id.loop.start);
            },
            .range => |range| {
                if (range.left != .identifier and range.left != .int_lit)
                    return self.fail(
                        "Expected an identifier or integer on left hand side",
                        range.token.start,
                    );
                if (range.right != .identifier and range.right != .int_lit)
                    return self.fail(
                        "Expected an identifier or integer on right hand side",
                        range.token.start,
                    );

                try self.compile(range.left);
                try self.compile(range.right);
                _ = try self.emit(.make_range);
            },
            .@"enum" => |enm| {
                var enums = try std.ArrayList([]const u8).initCapacity(self.allocator, enm.nodes.len);
                errdefer enums.deinit();

                for (enm.nodes) |n| {
                    if (n != .identifier)
                        return self.fail("Found a non-identifier inside the Enum declaration", enm.token.start);

                    enums.appendAssumeCapacity(n.identifier.value);
                }

                const pos = try self.addConstant(.{ ._enum = enums.toOwnedSlice() });
                _ = try self.emitOp(.load_const, pos);
            },
            // currently, switches operate on runtime until static types are implemented
            .switch_statement => |sw| {
                try self.compile(sw.capture);

                for (sw.prongs) |p, i| {
                    const prong = p.switch_prong;
                    // compile lhs of prong
                    try self.compile(prong.left);

                    // match with capture, capture is not popped from stack until the end
                    _ = try self.emit(.match);
                    const last_jump_false = try self.emit(.jump_false);
                    try self.compile(prong.right);
                    self.instructions.items[last_jump_false].ptr = @intCast(u16, self.instructions.items.len);
                }
                _ = try self.emit(.pop);
            },
            else => {},
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
            .opcodes = &[_]bytecode.Opcode{ .load_true, .not, .pop },
        },
        .{
            .input = "if (true) { 5 } 10",
            .consts = &[_]i64{ 5, 10 },
            .opcodes = &[_]bytecode.Opcode{
                .load_true,
                .jump_false,
                .load_const,
                .jump,
                .load_void,
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
                .get_by_index,
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
                .get_by_index,
                .pop,
            },
        },
        .{
            .input = "fn(){ 1 + 2 }",
            .consts = &[_]Value{ Value{ .integer = 1 }, Value{ .integer = 2 }, Value{ .function = .{ .arg_len = 0, .locals = 0, .instructions = undefined } } },
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                .load_const,
                .load_const,
                .add,
                .@"return",
                .load_const,
                .pop,
            },
        },
        .{
            .input = "fn(){ }",
            .consts = &[_]Value{Value{ .function = .{ .arg_len = 0, .locals = 0, .instructions = undefined } }},
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                .load_void,
                .@"return",
                .load_const,
                .pop,
            },
        },
        .{
            .input = "fn(){ 1 }()",
            .consts = &[_]Value{ Value{ .integer = 1 }, Value{ .function = .{ .arg_len = 0, .locals = 0, .instructions = undefined } } },
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                .load_const,
                .@"return",
                .load_const,
                .call,
                .pop,
            },
        },
        .{
            .input = "const x = fn(){ 1 } x()",
            .consts = &[_]Value{ Value{ .integer = 1 }, Value{ .function = .{ .arg_len = 0, .locals = 0, .instructions = undefined } } },
            .opcodes = &[_]bytecode.Opcode{
                .jump,
                .load_const,
                .@"return",
                .load_const,
                .bind_global,
                .load_global,
                .call,
                .pop,
            },
        },
        .{
            .input = "const x = 5 fn(){ return x }",
            .consts = &[_]Value{ Value{ .integer = 5 }, Value{ .function = .{ .arg_len = 0, .locals = 0, .instructions = undefined } } },
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
            .input = "fn(){ const x = 5 return x }",
            .consts = &[_]Value{ Value{ .integer = 5 }, Value{ .function = .{ .arg_len = 0, .locals = 1, .instructions = undefined } } },
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
            .input = "const func = fn(x){ return x } func(5)",
            .consts = &[_]Value{ Value{ .function = .{ .arg_len = 1, .locals = 1, .instructions = undefined } }, Value{ .integer = 5 } },
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
            .consts = &[_][]const u8{ "string", "len" },
            .opcodes = &[_]bytecode.Opcode{
                .load_const,
                .load_const,
                .get_by_index,
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
                .pop,
                .pop,
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
                .pop,
            },
        },
    };

    inline for (test_cases) |case, in| {
        var _errors = errors.Errors.init(testing.allocator);
        defer _errors.deinit();
        var code = try compile(testing.allocator, case.input, &_errors);
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
                    .function => {
                        testing.expectEqual(constant.function.arg_len, code.constants[i].function.arg_len);
                        testing.expectEqual(constant.function.locals, code.constants[i].function.locals);
                    },
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
