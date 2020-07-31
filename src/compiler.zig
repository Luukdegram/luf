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

    var compiler = Compiler{
        .instructions = bytecode.Instructions.init(allocator),
        .constants = Values.init(allocator),
        .allocator = allocator,
    };

    for (tree.nodes) |node| {
        try compiler.compile(node);
    }

    return Compiler.ByteCode{
        .instructions = compiler.instructions.toOwnedSlice(),
        .constants = compiler.constants.toOwnedSlice(),
        .allocator = allocator,
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

    /// Compiler errorset
    pub const Error = error{ CompilerError, OutOfMemory };

    /// Generated bytecode by the `Compiler`
    pub const ByteCode = struct {
        instructions: []const bytecode.Instruction,
        constants: []const Value,
        allocator: *Allocator,

        /// Frees all memory generated by the compiler
        pub fn deinit(self: ByteCode) void {
            self.allocator.free(self.instructions);
            self.allocator.free(self.constants);
        }
    };

    /// Adds a new constant to the compiler and returns the length of the list
    fn addConstant(self: *Compiler, val: Value) !u16 {
        try self.constants.append(val);
        return @truncate(u16, self.constants.items.len - 1);
    }

    /// Appends a new instruction to the compiler and returns the current position
    fn emit(self: *Compiler, op: bytecode.Opcode) !u16 {
        const pos = @truncate(u16, self.instructions.items.len);
        try self.instructions.append(bytecode.gen(op, null));
        return pos;
    }

    /// Appends a new instruction and saves the operand as bytes and returns the current position
    fn emitOp(self: *Compiler, op: bytecode.Opcode, operand: u16) !u16 {
        const pos = @truncate(u16, self.instructions.items.len);
        try self.instructions.append(bytecode.gen(op, operand));
        return pos;
    }

    fn compile(self: *Compiler, node: ast.Node) Error!void {
        switch (node) {
            .expression => |exp| {
                try self.compile(exp.value);
                _ = try self.emit(.pop);
            },
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
    };

    inline for (test_cases) |case| {
        const code = try compile(testing.allocator, case.input);
        defer code.deinit();

        testing.expect(case.consts.len == code.constants.len);
        for (case.consts) |int, i| {
            testing.expect(int == code.constants[i].integer);
        }
        for (case.opcodes) |op, i| {
            testing.expect(op == code.instructions[i].op);
        }
    }
}
