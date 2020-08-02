const std = @import("std");
const compiler = @import("compiler.zig");
const ByteCode = compiler.Compiler.ByteCode;
const byte_code = @import("bytecode.zig");
const Value = @import("value.zig").Value;
const Type = @import("value.zig").Type;
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
                .make_array => try self.analArray(inst),
                .make_map => try self.analMap(inst),
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

    /// Analyzes the instruction and builds an array
    fn analArray(self: *Vm, inst: byte_code.Instruction) Error!void {
        const len = inst.ptr;
        var list = try Value.List.initCapacity(&self.arena.allocator, len);
        errdefer list.deinit();

        if (len == 0) return self.push(.{ .list = list });

        var index = self.sp - len;
        var list_type: Type = undefined;
        var i: usize = 0;

        while (i < len) : ({
            i += 1;
            index += 1;
        }) {
            const val = self.stack[index];
            if (i == 0)
                list_type = std.meta.activeTag(val)
            else {
                if (list_type != std.meta.activeTag(val)) return Error.MissingValue;
            }
            try list.insert(i, val);
        }

        return self.push(.{ .list = list });
    }

    /// Analyzes and creates a new map
    fn analMap(self: *Vm, inst: byte_code.Instruction) Error!void {
        const len = inst.ptr / 2;
        var map = Value.Map.init(&self.arena.allocator);
        errdefer map.deinit();
        if (len == 0) return self.push(.{ .map = map });

        try map.ensureCapacity(len);

        var index = self.sp - (len * 2);
        var key_type: Type = undefined;
        var value_type: Type = undefined;
        var i: usize = 0;

        while (i < len) : ({
            i += 1;
            index += 2;
        }) {
            const key = self.stack[index];
            const value = self.stack[index + 1];

            if (i == 0) {
                key_type = std.meta.activeTag(key);
                value_type = std.meta.activeTag(value);
            } else {
                if (key_type != std.meta.activeTag(key)) return Error.MissingValue;
                if (value_type != std.meta.activeTag(value)) return Error.MissingValue;
            }

            map.putAssumeCapacity(key, value);
        }

        return self.push(.{ .map = map });
    }
};

/// Checks each given type if they are equal or not
fn resolveType(values: []Value) Vm.Error!Type {
    std.debug.assert(values.len > 0);
    const cur_tag: Type = std.meta.activeTag(values[0]);
    if (values.len == 1) return cur_tag;

    for (values[1..]) |value|
        if (std.meta.activeTag(value) != cur_tag) return Vm.Error.MissingValue;
}

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
        .{ .input = "\"foo\"", .expected = "foo" },
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

test "Arrays" {
    const test_cases = .{
        .{ .input = "[1, 2, 3]", .expected = &[_]i64{ 1, 2, 3 } },
        .{ .input = "[]", .expected = &[_]i64{} },
        .{ .input = "[1 + 1, 2 * 2, 6]", .expected = &[_]i64{ 2, 4, 6 } },
    };

    inline for (test_cases) |case| {
        const code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        const list = vm.popped().list;
        testing.expect(list.items.len == case.expected.len);
        inline for (case.expected) |int, i| {
            const items = list.items;
            testing.expectEqual(int, items[i].integer);
        }
    }
}

test "Maps" {
    const test_cases = .{
        .{ .input = "{1:2, 2:1, 5:6}", .expected = &[_]i64{ 2, 1, 6 }, .keys = &[_]i64{ 1, 2, 5 } },
        .{ .input = "{}", .expected = &[_]i64{}, .keys = &[_]i64{} },
        .{ .input = "{\"foo\":1}", .expected = &[_]i64{1}, .keys = &[_][]const u8{"foo"} },
    };

    inline for (test_cases) |case| {
        const code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        const map = vm.popped().map;
        testing.expect(map.items().len == case.expected.len);
        inline for (case.expected) |int, i| {
            const items = map.items();
            testing.expectEqual(int, items[i].value.integer);
        }

        inline for (case.keys) |key, i| {
            const item = map.items()[i];
            if (@TypeOf(key) == i64) {
                testing.expectEqual(key, item.key.integer);
            } else {
                testing.expectEqualStrings(key, item.key.string);
            }
        }
    }
}
