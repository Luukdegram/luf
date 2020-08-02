const std = @import("std");
const compiler = @import("compiler.zig");
const ByteCode = compiler.Compiler.ByteCode;
const byte_code = @import("bytecode.zig");
const Value = @import("value.zig").Value;
const testing = std.testing;
const Allocator = std.mem.Allocator;

//! The Virtual Machine of Luf is stack-based.
//! Currently the stack has a size of 2048 (pre-allocated)

/// Creates a new Virtual Machine on the stack, runs the given `ByteCode`
/// and finally returns the `Vm`. Use deinit() to free its memory.
pub fn run(code: ByteCode, allocator: *Allocator) Vm.Error!*Vm {
    //TODO cleanup this mess once we have implemented a garbage collector
    // to handle the memory
    const vm = try allocator.create(Vm);
    var arena = std.heap.ArenaAllocator.init(allocator);
    vm.* = .{
        .globals = Value.List.init(allocator),
        .allocator = allocator,
        .arena = arena,
    };
    try vm.run(code);
    return vm;
}

/// VM is a stack-based Virtual Machine
/// Although a register-based VM is more performant,
/// a stack-based one is more understandable
pub const Vm = struct {
    /// Stack pointer points to the next value
    sp: usize = 0,
    /// Stack has a max of 2048 Value's that it can hold
    /// This is pre-allocated.
    stack: [2048]Value = undefined,
    /// Globals that live inside the VM
    /// Currently allows 65536 Values
    globals: Value.List,
    allocator: *Allocator,
    arena: std.heap.ArenaAllocator,

    pub const Error = error{ OutOfMemory, MissingValue, InvalidOperator };

    /// Frees the Virtual Machine's memory
    pub fn deinit(self: *Vm) void {
        self.globals.deinit();
        self.arena.deinit();
        self.allocator.destroy(self);
    }

    /// Runs the given `ByteCode` on the Virtual Machine
    pub fn run(self: *Vm, code: ByteCode) Error!void {
        // instruction pointer
        var ip: usize = 0;
        while (ip < code.instructions.len) : (ip += 1) {
            const inst = code.instructions[ip];

            switch (inst.op) {
                .load_const => try self.push(code.constants[inst.ptr]),
                .equal, .not_equal, .less_than, .greater_than => try self.analCmp(inst.op),
                .add, .sub, .mul, .div => try self.analBinOp(inst.op),
                .pop => _ = self.pop(),
                .load_true => try self.push(Value.True),
                .load_false => try self.push(Value.False),
                .minus => try self.analNegation(),
                .bang => try self.analBang(),
                .load_nil => try self.push(Value.Nil),
                .jump => ip = inst.ptr - 1,
                .jump_false => {
                    const condition = self.pop().?;
                    if (!isTrue(condition)) ip = inst.ptr - 1;
                },
                .bind_global => try self.globals.append(self.pop().?),
                .load_global => try self.push(self.globals.items[inst.ptr]),
            }
        }
    }

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
        if (std.meta.activeTag(left) == std.meta.activeTag(right) and left == .string) {
            return self.analStringOp(op, left.string, right.string);
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

    fn analStringOp(self: *Vm, op: byte_code.Opcode, left: []const u8, right: []const u8) Error!void {
        if (op != .add) return Error.InvalidOperator;

        return self.push(.{ .string = try std.mem.concat(&self.arena.allocator, u8, &[_][]const u8{ left, right }) });
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
            .equal => try self.push(if (left.boolean == right.boolean) Value.True else Value.False),
            .not_equal => try self.push(if (left.boolean != right.boolean) Value.True else Value.False),
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

        return self.push(if (boolean) Value.True else Value.False);
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
            .nil => true,
            else => false,
        };
        return self.push(if (val) Value.True else Value.False);
    }
};

/// Evalutes if the given `Value` is truthy.
/// Only accepts booleans and 'Nil'.
fn isTrue(value: Value) bool {
    return switch (value) {
        .nil => false,
        .boolean => |val| val,
        else => true,
    };
}

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
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
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
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
        testing.expect(case.expected == vm.popped().boolean);
    }
}

test "Conditional" {
    const test_cases = .{
        .{ .input = "if (true) { 10 }", .expected = 10 },
        .{ .input = "if (true) { 10 } else { 20 }", .expected = 10 },
        .{ .input = "if (false) { 10 } else { 20 }", .expected = 20 },
        .{ .input = "if (1 < 2) { 10 }", .expected = 10 },
        .{ .input = "if (1 > 2) { 10 }", .expected = null },
    };

    inline for (test_cases) |case| {
        const code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
        if (@TypeOf(case.expected) == comptime_int) {
            testing.expect(case.expected == vm.popped().integer);
        } else {
            testing.expect(vm.popped() == .nil);
        }
    }
}

test "Declaration" {
    const test_cases = .{
        .{ .input = "const x = 1 x", .expected = 1 },
        .{ .input = "const x = 1 const y = 1 x + y", .expected = 2 },
        .{ .input = "mut x = 1 const y = x + x x + y", .expected = 3 },
    };

    inline for (test_cases) |case| {
        const code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
        testing.expect(case.expected == vm.popped().integer);
    }
}

test "Strings" {
    const test_cases = .{
        //.{ .input = "\"foo\"", .expected = "foo" },
        .{ .input = "\"foo\" + \"bar\"", .expected = "foobar" },
    };

    inline for (test_cases) |case| {
        const code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
        testing.expectEqualStrings(case.expected, vm.popped().string);
    }
}
