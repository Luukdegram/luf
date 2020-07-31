const std = @import("std");
const compiler = @import("compiler.zig");
const ByteCode = compiler.Compiler.ByteCode;
const byte_code = @import("bytecode.zig");
const Value = @import("value.zig").Value;
const testing = std.testing;

//! The Virtual Machine of Luf is stack-based.
//! Currently the stack has a size of 2048 (pre-allocated)

pub fn run(code: ByteCode) Vm.Error!Vm {
    var vm = Vm{};

    // instruction pointer
    var ip: usize = 0;
    for (code.instructions) |inst| {
        switch (inst.op) {
            .load_const => {
                try vm.push(code.constants[inst.ptr]);
            },
            .equal, .not_equal, .less_than, .greater_than => try vm.analCmp(inst.op),
            .add, .sub, .mul, .div => try vm.analBinOp(inst.op),
            .pop => _ = vm.pop(),
            .load_true => try vm.push(.{ .boolean = true }),
            .load_false => try vm.push(.{ .boolean = false }),
            .minus => try vm.analNegation(),
            .bang => try vm.analBang(),
            else => std.debug.panic("TODO Implement operator: {}", .{inst.op}),
        }
    }

    return vm;
}

/// VM is a stack-based Virtual Machine
/// Although a register-based VM is more performant,
/// a stack-based one is more understandable
pub const Vm = struct {
    /// Stack pointer holds the current stack position
    sp: usize = 0,
    /// Stack has a max of 2048 Value's that it can hold
    /// This is pre-allocated.
    stack: [2048]Value = undefined,

    pub const Error = error{ OutOfMemory, MissingValue, InvalidOperator };

    /// Pushes a new `Value` on top of the `stack` and increases
    /// the stack pointer by 1.
    fn push(self: *Vm, value: Value) Error!void {
        if (self.sp > self.stack.len - 1) return Error.OutOfMemory;

        self.stack[self.sp] = value;
        self.sp += 1;
    }

    /// Returns the `Value` from the `stack` and decreases the stack
    /// pointer by 1. Returns null if stack is empty.
    fn pop(self: *Vm) ?Value {
        if (self.sp == 0) return null;
        defer self.sp -= 1;
        return self.stack[self.sp - 1];
    }

    /// Returns the previously popped `Value`
    /// Note that this results in UB if the stack is empty.
    fn popped(self: *Vm) Value {
        return self.stack[self.sp];
    }

    /// Analyzes and executes a binary operation.
    fn analBinOp(self: *Vm, op: byte_code.Opcode) Error!void {
        // right is on the top of left, therefore we pop it first
        const right = self.pop() orelse return Error.MissingValue;
        const left = self.pop() orelse return Error.MissingValue;

        if (std.meta.activeTag(left) == std.meta.activeTag(right) and left == .integer) {
            return self.analIntOp(op, left.integer, right.integer);
        }
    }

    /// Analyzes and executes a binary operation on an integer
    fn analIntOp(self: *Vm, op: byte_code.Opcode, left: i64, right: i64) Error!void {
        const result = switch (op) {
            .add => left + right,
            .sub => left - right,
            .mul => left * right,
            .div => @divTrunc(left, right),
            else => return Error.InvalidOperator,
        };

        return self.push(.{ .integer = result });
    }

    /// Analyzes a then executes a comparison and pushes the return value on the stack
    fn analCmp(self: *Vm, op: byte_code.Opcode) Error!void {
        // right is on the top of left, therefore we pop it first
        const right = self.pop() orelse return Error.MissingValue;
        const left = self.pop() orelse return Error.MissingValue;

        if (std.meta.activeTag(left) == std.meta.activeTag(right) and left == .integer) {
            return self.analIntCmp(op, left.integer, right.integer);
        }

        // for now just assume it's a boolean
        switch (op) {
            .equal => try self.push(.{ .boolean = left.boolean == right.boolean }),
            .not_equal => try self.push(.{ .boolean = left.boolean != right.boolean }),
            else => return Error.InvalidOperator,
        }
    }

    /// Analyzes and compares 2 integers depending on the given operator
    fn analIntCmp(self: *Vm, op: byte_code.Opcode, left: i64, right: i64) Error!void {
        const boolean = switch (op) {
            .equal => left == right,
            .not_equal => left != right,
            .greater_than => left > right,
            .less_than => left < right,
            else => return Error.InvalidOperator,
        };

        return self.push(.{ .boolean = boolean });
    }

    /// Analyzes and executes a negation
    fn analNegation(self: *Vm) Error!void {
        const right = self.pop() orelse return Error.MissingValue;
        if (right != .integer) return Error.InvalidOperator;

        return self.push(.{ .integer = -right.integer });
    }
    /// Analyzes and executes the '!' operator
    fn analBang(self: *Vm) Error!void {
        const right = self.pop() orelse return Error.MissingValue;

        const val = switch (right) {
            .boolean => !right.boolean,
            else => false,
        };
        return self.push(.{ .boolean = val });
    }
};

test "Integer arithmetic" {
    const test_cases = .{
        .{ .input = "1", .expected = 1 },
        .{ .input = "2", .expected = 2 },
        .{ .input = "1 + 1", .expected = 2 },
        .{ .input = "1 * 3", .expected = 3 },
        .{ .input = "1 - 3", .expected = -2 },
        .{ .input = "10 / 2", .expected = 5 },
        .{ .input = "-2", .expected = -2 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
    };

    inline for (test_cases) |case| {
        const code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code);
        testing.expect(case.expected == vm.popped().integer);
    }
}

test "Boolean" {
    const test_cases = .{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "1 < 2", .expected = true },
        .{ .input = "1 > 2", .expected = false },
        .{ .input = "1 == 1", .expected = true },
        .{ .input = "1 != 1", .expected = false },
        .{ .input = "true != true", .expected = false },
        .{ .input = "!true", .expected = false },
    };

    inline for (test_cases) |case| {
        const code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code);
        testing.expect(case.expected == vm.popped().boolean);
    }
}
