const std = @import("std");
const compiler = @import("compiler.zig");
const ByteCode = compiler.Compiler.ByteCode;
const byte_code = @import("bytecode.zig");
const _value = @import("value.zig");
const Value = _value.Value;
const Type = _value.Type;
const BuiltinError = _value.BuiltinError;
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
        .call_stack = Vm.CallStack.init(allocator),
    };

    try vm.call_stack.append(.{
        .fp = undefined,
        .sp = 0,
        .ip = 0,
        .instructions = code.instructions,
    });
    errdefer vm.deinit();
    try vm.run(code);
    return vm;
}

/// VM is a stack-based Virtual Machine
/// Although a register-based VM is more performant,
/// a stack-based one is more understandable
pub const Vm = struct {
    /// Stack pointer points to the next value
    sp: usize = 0,
    /// Instruction pointer. Points to current instruction of the loaded instruction set
    ip: usize = 0,
    /// Stack has a max of 2048 Value's that it can hold
    /// This is pre-allocated.
    stack: [2048]*Value = undefined,
    /// Globals that live inside the VM
    /// Currently allows 65536 Values
    globals: Value.List,
    allocator: *Allocator,
    arena: std.heap.ArenaAllocator,
    call_stack: CallStack,

    pub const Error = error{ OutOfMemory, OutOfBounds, MissingValue, InvalidOperator } || BuiltinError;
    const CallStack = std.ArrayList(Frame);
    /// Function `Frame` on the callstack
    const Frame = struct {
        /// Frame pointer which contains the actual function `Value`
        fp: *const Value,
        /// `Instruction` pointer to the position of the function in the bytecode's instruction set
        ip: usize,
        /// Stack pointer. Mostly used to reset the stack pointer between scope changes
        sp: usize,
        /// Instructions to run on the current call stack
        instructions: []const byte_code.Instruction,
    };

    /// Frees the Virtual Machine's memory
    pub fn deinit(self: *Vm) void {
        self.globals.deinit();
        self.arena.deinit();
        self.call_stack.deinit();
        self.allocator.destroy(self);
    }

    /// Runs the given `ByteCode` on the Virtual Machine
    pub fn run(self: *Vm, code: ByteCode) Error!void {
        while (self.frame().ip < self.frame().instructions.len) {
            var current_frame = self.frame();
            defer current_frame.ip += 1;
            const inst = current_frame.instructions[current_frame.ip];

            switch (inst.op) {
                .load_const => {
                    const val = try self.newValue();
                    val.* = code.constants[inst.ptr];
                    try self.push(val);
                },
                .equal,
                .not_equal,
                .less_than,
                .greater_than,
                .@"and",
                .@"or",
                => try self.analCmp(inst.op),
                .add,
                .sub,
                .mul,
                .div,
                .mod,
                .bitwise_and,
                .bitwise_or,
                .bitwise_xor,
                .shift_left,
                .shift_right,
                => try self.analBinOp(inst.op),
                .assign_add,
                .assign_div,
                .assign_mul,
                .assign_sub,
                => try self.analAssignAndBinOp(inst.op),
                .pop => _ = self.pop(),
                .load_true => try self.push(&Value.True),
                .load_false => try self.push(&Value.False),
                .minus => try self.analNegation(),
                .not => try self.analNot(),
                .bitwise_not => try self.analBitwiseNot(),
                .load_nil => try self.push(&Value.Nil),
                .jump => current_frame.ip = inst.ptr - 1,
                .jump_false => {
                    const condition = self.pop().?;
                    if (!isTrue(condition)) current_frame.ip = inst.ptr - 1;
                },
                .bind_global => {
                    const val = self.pop().?;
                    if (self.globals.items.len > inst.ptr)
                        self.globals.items[inst.ptr] = val
                    else
                        try self.globals.append(val);
                },
                .load_global => try self.push(self.globals.items[inst.ptr]),
                .make_array => try self.analArray(inst),
                .make_map => try self.analMap(inst),
                .get_by_index => try self.analGetValue(),
                .set_by_index => try self.analSetValue(),
                ._return => {
                    const f = self.call_stack.pop();
                    self.sp = f.sp;

                    //_ = self.pop();
                    try self.push(&Value.Nil);
                },
                .return_value => {
                    const rv = self.pop().?;

                    // remove the function frame from the call stack
                    const f = self.call_stack.pop();
                    self.sp = f.sp - 1;

                    // get return value from stack

                    // remove function itself from stack
                    //_ = self.pop();

                    // push the return value back to the stack
                    try self.push(rv);
                },
                .call => try self.analFunctionCall(inst, current_frame.instructions[current_frame.ip + 1]),
                .bind_local => self.stack[current_frame.sp + inst.ptr] = self.pop().?,
                .load_local => try self.push(self.stack[current_frame.sp + inst.ptr]),
                .load_builtin => {
                    const key = Value.builtin_keys[inst.ptr];
                    try self.push(&(Value.builtins.get(key).?));
                },
                .assign_global => {
                    const value = self.pop().?;
                    if (self.globals.items.len > inst.ptr)
                        self.globals.items[inst.ptr] = value
                    else
                        try self.globals.append(value);
                },
                .assign_local => self.stack[current_frame.sp + inst.ptr] = self.pop().?,
                .load_module => try self.loadModule(),
                else => {},
            }
        }
    }

    /// Pushes a new `Value` on top of the `stack` and increases
    /// the stack pointer by 1.
    fn push(self: *Vm, value: *Value) Error!void {
        if (self.sp >= self.stack.len - 1) return Error.OutOfMemory;

        self.stack[self.sp] = value;
        self.sp += 1;
    }

    /// Returns the `Value` from the `stack` and decreases the stack
    /// pointer by 1. Returns null if stack is empty.
    fn pop(self: *Vm) ?*Value {
        if (self.sp == 0) return null;
        self.sp -= 1;
        return self.stack[self.sp];
    }

    /// Returns the previously popped `Value`
    /// Note that this results in UB if the stack is empty.
    fn peek(self: *Vm) *Value {
        return self.stack[self.sp];
    }

    /// Returns the current `Frame` of the call stack
    /// Asserts the call stack is not empty
    fn frame(self: *Vm) *Frame {
        return &self.call_stack.items[(self.call_stack.items.len - 1)];
    }

    /// Analyzes and executes a binary operation.
    fn analBinOp(self: *Vm, op: byte_code.Opcode) Error!void {
        // right is on the top of left, therefore we pop it first
        const right = self.pop() orelse return Error.MissingValue;
        const left = self.pop() orelse return Error.MissingValue;

        const luf_type = try resolveType(&[_]*Value{ left, right });

        return switch (luf_type) {
            .integer => self.analIntOp(op, left.integer, right.integer),
            .string => self.analStringOp(op, left.string, right.string),
            else => Error.InvalidOperator,
        };
    }

    /// Analyzes and executes a binary operation on an integer
    fn analIntOp(self: *Vm, op: byte_code.Opcode, left: i64, right: i64) Error!void {
        const result = switch (op) {
            .add => left + right,
            .sub => left - right,
            .mul => left * right,
            .mod => if (right > 0) @mod(left, right) else return Error.InvalidOperator,
            .div => blk: {
                if (right == 0) return Error.InvalidOperator;
                break :blk @divTrunc(left, right);
            },
            .bitwise_and => left & right,
            .bitwise_xor => left ^ right,
            .bitwise_or => left | right,
            .shift_left => blk: {
                if (right < 0) return Error.InvalidOperator;
                break :blk if (right > std.math.maxInt(u6)) 0 else left << @intCast(u6, right);
            },
            .shift_right => blk: {
                if (right < 0) return Error.InvalidOperator;
                break :blk if (right > std.math.maxInt(u6)) 0 else left >> @intCast(u6, right);
            },
            else => return Error.InvalidOperator,
        };

        const res = try self.newValue();
        res.* = .{ .integer = result };
        return self.push(res);
    }

    /// Concats two strings together
    fn analStringOp(self: *Vm, op: byte_code.Opcode, left: []const u8, right: []const u8) Error!void {
        if (op != .add) return Error.InvalidOperator;

        const res = try self.newValue();
        res.* = .{ .string = try std.mem.concat(&self.arena.allocator, u8, &[_][]const u8{ left, right }) };
        return self.push(res);
    }

    /// Analyzes the instruction, ensures the lhs is an identifier and the rhs is an integer or string.
    /// Strings are only valid for the `assign_add` bytecode instruction
    fn analAssignAndBinOp(self: *Vm, op: byte_code.Opcode) Error!void {
        const right = self.pop() orelse return Error.MissingValue;
        const left = self.pop() orelse return Error.MissingValue;

        if (left.lufType() != right.lufType()) return Error.InvalidOperator;

        if (left.isType(.integer)) {
            try self.analIntOp(switch (op) {
                .assign_add => .add,
                .assign_div => .div,
                .assign_mul => .mul,
                .assign_sub => .sub,
                else => unreachable,
            }, left.integer, right.integer);
            const val = self.pop().?;
            left.* = val.*;
            return self.push(&Value.Nil);
        }
        if (left.isType(.string)) {
            if (op != .assign_add) return Error.InvalidOperator;

            try self.analStringOp(.add, left.string, right.string);
            const val = self.pop().?;
            left.* = val.*;
            return self.push(&Value.Nil);
        }

        return Error.InvalidOperator;
    }

    /// Analyzes a then executes a comparison and pushes the return value on the stack
    fn analCmp(self: *Vm, op: byte_code.Opcode) Error!void {
        // right is on the top of left, therefore we pop it first
        const right = self.pop() orelse return Error.MissingValue;
        const left = self.pop() orelse return Error.MissingValue;

        if (left.lufType() == right.lufType() and left.isType(.integer)) {
            return self.analIntCmp(op, left.integer, right.integer);
        }

        if (left.lufType() == right.lufType() and left.isType(.string)) {
            return self.analStringCmp(op, left.string, right.string);
        }

        if (!left.isType(.boolean) or !right.isType(.boolean)) return Error.InvalidOperator;

        return switch (op) {
            .equal => self.push(if (left.boolean == right.boolean) &Value.True else &Value.False),
            .not_equal => self.push(if (left.boolean != right.boolean) &Value.True else &Value.False),
            .@"and" => self.push(if (left.boolean and right.boolean) &Value.True else &Value.False),
            .@"or" => self.push(if (left.boolean or right.boolean) &Value.True else &Value.False),
            else => Error.InvalidOperator,
        };
    }

    /// Analyzes and compares 2 integers depending on the given operator
    fn analIntCmp(self: *Vm, op: byte_code.Opcode, left: i64, right: i64) Error!void {
        const boolean = switch (op) {
            .equal => left == right,
            .not_equal => left != right,
            .greater_than => left > right,
            .greater_than_equal => left >= right,
            .less_than => left < right,
            .less_than_equal => left <= right,
            else => return Error.InvalidOperator,
        };

        return self.push(if (boolean) &Value.True else &Value.False);
    }

    /// Analyzes and compares 2 strings
    fn analStringCmp(self: *Vm, op: byte_code.Opcode, left: []const u8, right: []const u8) Error!void {
        var eql = std.mem.eql(u8, left, right);
        switch (op) {
            .equal => {},
            .not_equal => eql = !eql,
            else => return Error.InvalidOperator,
        }

        return self.push(if (eql) &Value.True else &Value.False);
    }

    /// Analyzes and executes a negation
    fn analNegation(self: *Vm) Error!void {
        const right = self.pop() orelse return Error.MissingValue;
        if (right.* != .integer) return Error.InvalidOperator;

        const res = try self.newValue();
        res.* = .{ .integer = -right.integer };
        return self.push(res);
    }
    /// Analyzes and executes the '!' operator
    fn analNot(self: *Vm) Error!void {
        const right = self.pop() orelse return Error.MissingValue;

        const val = switch (right.*) {
            .boolean => !right.boolean,
            .nil => true,
            else => false,
        };
        return self.push(if (val) &Value.True else &Value.False);
    }

    /// Executes the ~ operator
    fn analBitwiseNot(self: *Vm) Error!void {
        const value = self.pop() orelse return Error.MissingValue;

        if (!value.isType(.integer)) return Error.InvalidOperator;

        const ret = try self.newValue();
        ret.* = .{ .integer = ~value.integer };
        return self.push(ret);
    }

    /// Analyzes the instruction and builds an array
    fn analArray(self: *Vm, inst: byte_code.Instruction) Error!void {
        const len = inst.ptr;
        var list = try Value.List.initCapacity(&self.arena.allocator, len);
        errdefer list.deinit();

        const res = try self.newValue();
        if (len == 0) {
            res.* = .{ .list = list };
            return self.push(res);
        }

        var index = self.sp - len;
        var list_type: Type = undefined;
        var i: usize = 0;

        while (i < len) : ({
            i += 1;
            index += 1;
        }) {
            const val = self.stack[index];
            if (i == 0)
                list_type = val.lufType()
            else if (!val.isType(list_type)) return Error.MissingValue;

            try list.insert(i, val);
        }

        res.* = .{ .list = list };
        return self.push(res);
    }

    /// Analyzes and creates a new map
    fn analMap(self: *Vm, inst: byte_code.Instruction) Error!void {
        const len = inst.ptr / 2;
        var map = Value.Map.init(&self.arena.allocator);
        errdefer map.deinit();

        const res = try self.newValue();
        if (len == 0) {
            res.* = .{ .map = map };
            return self.push(res);
        }

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
                key_type = key.lufType();
                value_type = value.lufType();
            } else {
                if (!key.isType(key_type)) return Error.MissingValue;
                if (!value.isType(value_type)) return Error.MissingValue;
            }

            map.putAssumeCapacity(key, value);
        }

        res.* = .{ .map = map };
        return self.push(res);
    }

    /// Analyzes an index/reference pushes the value on the stack
    fn analGetValue(self: *Vm) Error!void {
        const index = self.pop() orelse return Error.MissingValue;
        const left = self.pop() orelse return Error.MissingValue;

        switch (left.*) {
            .list => |list| {
                switch (index.*) {
                    .integer => |int| {
                        if (int < 0 or int > list.items.len) return Error.OutOfBounds;
                        return self.push(list.items[@intCast(u64, int)]);
                    },
                    .string => |name| {
                        if (Value.builtins.get(name)) |val| {
                            const builtin = val.native;
                            const args = try self.allocator.alloc(*Value, builtin.arg_len + 1);
                            defer self.allocator.free(args);
                            args[0] = left;
                            var i: usize = 1;
                            while (i <= builtin.arg_len) : (i += 1) args[i] = self.stack[self.sp - (builtin.arg_len - 1 + i)];
                            const result = (try builtin.func(args)).*;

                            //create a shallow copy
                            const res = try self.newValue();
                            res.* = result;

                            return self.push(res);
                        }
                    },
                    else => return error.InvalidOperator,
                }
            },
            .map => |map| {
                if (map.get(index)) |val| {
                    return self.push(val);
                }
                // We return null to the user so they have something to check against
                // to see if a key exists or not.
                return self.push(&Value.Nil);
            },
            .string => |string| {
                switch (index.*) {
                    .integer => |int| {
                        if (int < 0 or int > string.len) return Error.OutOfBounds;

                        const val = try self.newValue();
                        val.* = .{ .string = string[@intCast(usize, int)..@intCast(usize, int + 1)] };
                        return self.push(val);
                    },
                    .string => |name| {
                        if (Value.builtins.get(name)) |val| {
                            const builtin = val.native;
                            const result = (try builtin.func(&[_]*Value{left})).*;

                            //create a shallow copy
                            const res = try self.newValue();
                            res.* = result;

                            return self.push(res);
                        }
                    },
                    else => return Error.InvalidOperator,
                }
            },
            .module => |mod| {
                if (!index.isType(.string)) return error.InvalidOperator;

                // Find the definition in the module, if it doesn't exist, it's an error.
                // this differs from a regular map where we return null
                if (mod.attributes.map.get(index)) |val| {
                    return self.push(val);
                } else {
                    return error.MissingValue;
                }
            },
            else => return error.InvalidOperator,
        }
    }

    /// Sets the lhs by the value on top of the current stack
    /// This also does type checking to ensure the lhs and rhs are equal types
    fn analSetValue(self: *Vm) Error!void {
        const value = self.pop() orelse return Error.MissingValue;
        const right = self.pop() orelse return Error.MissingValue;
        const left = self.pop() orelse return Error.MissingValue;

        if (left.isType(.list) and right.isType(.integer)) {
            const list = left.list;
            if (right.integer < 0 or right.integer > list.items.len) return Error.OutOfBounds;
            list.items[@intCast(usize, right.integer)].* = value.*;
            return self.push(&Value.Nil);
        } else if (left.isType(.map)) {
            const map = left.map;
            if (map.get(right)) |val| {
                val.* = value.*;
                return self.push(&Value.Nil);
            } else {
                // replace with more descriptive Error
                return Error.MissingValue;
            }
        }

        // replace with more descriptive Error
        return Error.MissingValue;
    }

    /// Analyzes the current instruction to execture a function call
    /// Expects the current instruction pointer.
    /// This will also return the new instruction pointer
    ///
    /// `instructions` is required to detect if we can optimize tail recursion
    fn analFunctionCall(self: *Vm, inst: byte_code.Instruction, next: byte_code.Instruction) Error!void {
        const arg_len = inst.ptr;
        const val = self.stack[self.sp - (1 + arg_len)];

        // if it's not a function call, we can expect it to be a builtin function
        if (val.* != .function) return;

        if (arg_len != val.function.arg_len) return Error.MissingValue;

        var cur_frame = self.frame();
        if (cur_frame.fp == val and next.op == .return_value) {
            var i: usize = 0;
            while (i < arg_len) : (i += 1)
                self.stack[cur_frame.sp + i] = self.stack[self.sp - arg_len + i];
            cur_frame.ip = 0;
            return;
        }

        try self.call_stack.append(.{
            .fp = val,
            .ip = 0,
            .sp = self.sp - arg_len,
            .instructions = val.function.instructions,
        });

        self.sp = self.frame().sp + val.function.locals + 1;
    }

    /// Analyzes for argument length and then calls the builtin function
    /// Returns the result on the stack
    fn analBuiltinFunctionCall(self: *Vm, val: *const Value) Error!void {
        std.debug.assert(val.* == .native);

        // insert the arguments lower on the stack
        // so they can be called correctly by analIndex
        var i: usize = 0;
        while (i < val.native.arg_len) : (i += 1)
            self.stack[self.sp - val.native.arg_len - i] = self.pop().?;
    }

    /// Reads the file name, open it, and compile its source code.
    /// After compilator, the symbols will be saved in a map and returned
    fn importModule(self: *Vm, file_name: []const u8) Error!*Value {
        const file = std.fs.cwd().openFile(file_name, .{}) catch {
            return Error.InvalidOperator;
        };
        defer file.close();
        const size = file.getEndPos() catch {
            return Error.InvalidOperator;
        };

        const source = file.readAllAlloc(&self.arena.allocator, size, size) catch {
            return Error.OutOfMemory;
        };
        //defer self.allocator.free(source);
        var code = compiler.compile(&self.arena.allocator, source) catch {
            return Error.InvalidOperator;
        };
        //defer code.deinit();
        const last_ip = self.ip;
        const last_sp = self.sp;

        self.ip = 0;
        //self.sp = 0;

        try self.run(code);

        // Get all constants exposed by the source file
        var attributes = Value.Map.init(&self.arena.allocator);
        try attributes.ensureCapacity(code.symbols.items().len);
        for (code.symbols.items()) |entry, i| {
            // all builtins are stored as symbols first, so skip those symbols
            if (i >= Value.builtin_keys.len) {
                const symbol: compiler.Compiler.Symbol = entry.value;
                std.debug.assert(symbol.scope == .root);

                const name = try self.newValue();
                name.* = .{ .string = try self.arena.allocator.dupe(u8, symbol.name) };
                const value = try self.newValue();
                value.* = code.constants[symbol.index];
                attributes.putAssumeCapacityNoClobber(name, value);
            }
        }
        self.ip = last_ip;
        //self.sp = last_sp + 1;

        const ret = try self.newValue();
        ret.* = .{ .map = attributes };
        return ret;
    }

    /// Loads a module, if not existing, compiles the source code of the given file name
    fn loadModule(self: *Vm) Error!void {
        const name = self.pop().?;
        if (!name.isType(.string)) return Error.InvalidOperator;

        const mod = try self.importModule(name.string);

        const ret = try self.newValue();
        ret.* = .{
            .module = .{
                .name = name.string,
                .attributes = mod,
            },
        };

        return self.push(ret);
    }

    /// Creates a new Value on the heap which will be freed on scope exits (call_stack pops)
    fn newValue(self: *Vm) !*Value {
        return self.arena.allocator.create(Value);
    }
};

/// Checks each given type if they are equal or not
fn resolveType(values: []*const Value) Vm.Error!Type {
    std.debug.assert(values.len > 0);
    const cur_tag: Type = values[0].lufType();
    if (values.len == 1) return cur_tag;

    for (values[1..]) |value|
        if (!value.isType(cur_tag)) return Vm.Error.InvalidOperator;

    return cur_tag;
}

/// Evalutes if the given `Value` is truthy.
/// Only accepts booleans and 'Nil'.
fn isTrue(value: *const Value) bool {
    return switch (value.*) {
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
        .{ .input = "10 % 2", .expected = 0 },
        .{ .input = "1 | 2", .expected = 3 },
        .{ .input = "2 ^ 4", .expected = 6 },
        .{ .input = "3 & 6", .expected = 2 },
        .{ .input = "-2", .expected = -2 },
        .{ .input = "1 << 2", .expected = 4 },
        .{ .input = "4 >> 2", .expected = 1 },
        .{ .input = "~1", .expected = -2 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
        .{ .input = "mut x = 0 x+= 1 x", .expected = 1 },
        .{ .input = "mut x = 2 x*= 2 x", .expected = 4 },
        .{ .input = "mut x = 10 x/= 2 x", .expected = 5 },
        .{ .input = "mut x = 1 x-= 1 x", .expected = 0 },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
        testing.expect(case.expected == vm.peek().integer);
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
        .{ .input = "true and true", .expected = true },
        .{ .input = "true and false", .expected = false },
        .{ .input = "false and false", .expected = false },
        .{ .input = "true or false", .expected = true },
        .{ .input = "true or true", .expected = true },
        .{ .input = "false or false", .expected = false },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
        testing.expect(case.expected == vm.peek().boolean);
    }
}

test "Conditional" {
    const test_cases = .{
        .{ .input = "if (true) { 10 }", .expected = 10 },
        .{ .input = "if (true) { 10 } else { 20 }", .expected = 10 },
        .{ .input = "if (false) { 10 } else { 20 }", .expected = 20 },
        .{ .input = "if (1 < 2) { 10 }", .expected = 10 },
        .{ .input = "if (1 > 2) { 10 }", .expected = null },
        .{ .input = "if (1 > 2) { 10 } else if (2 > 3) { 20 } else { 5 }", .expected = 5 },
        .{ .input = "if (1 > 2) { 10 } else if (2 < 3) { 20 } else { 5 }", .expected = 20 },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
        if (@TypeOf(case.expected) == comptime_int) {
            testing.expect(case.expected == vm.peek().integer);
        } else {
            testing.expect(vm.peek().* == .nil);
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
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
        testing.expect(case.expected == vm.peek().integer);
    }
}

test "Strings" {
    const test_cases = .{
        .{ .input = "\"foo\"", .expected = "foo" },
        .{ .input = "\"foo\" + \"bar\"", .expected = "foobar" },
        .{ .input = "const x = \"foo\" x+=\"bar\" x", .expected = "foobar" },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();
        testing.expectEqualStrings(case.expected, vm.peek().string);
    }
}

test "Arrays" {
    const test_cases = .{
        .{ .input = "[1, 2, 3]", .expected = &[_]i64{ 1, 2, 3 } },
        .{ .input = "[]", .expected = &[_]i64{} },
        .{ .input = "[1 + 1, 2 * 2, 6]", .expected = &[_]i64{ 2, 4, 6 } },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        const list = vm.peek().list;
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
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        const map = vm.peek().map;
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

test "Index" {
    const test_cases = .{
        .{ .input = "[1, 2, 3][1]", .expected = 2 },
        .{ .input = "const list = [1, 2, 3] list[1] = 10 list[1]", .expected = 10 },
        .{ .input = "{1: 5}[1]", .expected = 5 },
        .{ .input = "{2: 5}[0]", .expected = &Value.Nil },
        .{ .input = "{2: 5}[2] = 1", .expected = &Value.Nil },
        .{ .input = "const map = {2: 5} map[2] = 1 map[2]", .expected = 1 },
        .{ .input = "{\"foo\": 15}[\"foo\"]", .expected = 15 },
        .{ .input = "\"hello\"[1]", .expected = "e" },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        if (@TypeOf(case.expected) == comptime_int)
            testing.expectEqual(@as(i64, case.expected), vm.peek().integer)
        else if (@TypeOf(case.expected) == *const [1:0]u8)
            testing.expectEqualStrings(case.expected, vm.peek().string)
        else
            testing.expectEqual(case.expected, vm.peek());
    }
}

test "Basic function calls with no arguments" {
    const test_cases = .{
        .{ .input = "const x = fn() { return 1 + 2 } x()", .expected = 3 },
        .{ .input = "const x = fn() { return 1 } const y = fn() { return 5 } x() + y()", .expected = 6 },
        .{ .input = "const x = fn() { return 5 10 } x()", .expected = 5 },
        .{ .input = "const x = fn() { } x()", .expected = &Value.Nil },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        if (@TypeOf(case.expected) == comptime_int)
            testing.expectEqual(@as(i64, case.expected), vm.peek().integer)
        else
            testing.expectEqual(case.expected, vm.peek());
    }
}

test "Globals vs Locals" {
    const test_cases = .{
        .{ .input = "const x = fn() { const x = 5 return x } x()", .expected = 5 },
        .{ .input = "const x = fn() { const y = 1 const z = 2 return y + z } x()", .expected = 3 },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        if (@TypeOf(case.expected) == comptime_int)
            testing.expectEqual(@as(i64, case.expected), vm.peek().integer)
        else
            testing.expectEqual(case.expected, vm.peek().*);
    }
}

test "Functions with arguments" {
    const test_cases = .{
        .{ .input = "const x = fn(x) { return x } x(3)", .expected = 3 },
        .{ .input = "const x = fn(a, b) { return a + b } x(3,5)", .expected = 8 },
        .{ .input = "const x = fn(a, b) { const z = a + b return z } x(3,5)", .expected = 8 },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        if (@TypeOf(case.expected) == comptime_int)
            testing.expectEqual(@as(i64, case.expected), vm.peek().integer)
        else
            testing.expectEqual(case.expected, vm.peek());
    }
}

test "Builtins" {
    const test_cases = .{
        .{ .input = "\"Hello world\".len", .expected = 11 },
        .{ .input = "[1,5,2].len", .expected = 3 },
        .{ .input = "const x = [1] x.add(2) x.len", .expected = 2 },
        .{ .input = "const x = [1, 2] x.pop() x.len", .expected = 1 },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        testing.expectEqual(@as(i64, case.expected), vm.peek().integer);
    }
}

test "While loop" {
    const test_cases = .{
        .{ .input = "mut i = 0 while (i > 10) {mut i = 10} i", .expected = 0 },
        .{ .input = "mut i = 0 while (i < 10) {i = 10} i", .expected = 10 },
    };

    inline for (test_cases) |case| {
        var code = try compiler.compile(testing.allocator, case.input);
        defer code.deinit();
        var vm = try run(code, testing.allocator);
        defer vm.deinit();

        testing.expectEqual(@as(i64, case.expected), vm.peek().integer);
    }
}

test "Tail recursion" {
    const input =
        \\const func = fn(a) {
        \\  if (a == 10) {
        \\      return a
        \\  }
        \\  return func(a + 1)  
        \\}
        \\func(2) 
    ;
    var code = try compiler.compile(testing.allocator, input);
    defer code.deinit();
    var vm = try run(code, testing.allocator);
    defer vm.deinit();

    testing.expectEqual(@as(i64, 10), vm.peek().integer);
}

test "Imports" {
    const input = "const imp = import(\"test/test.luf\")";
    var code = try compiler.compile(testing.allocator, input);
    defer code.deinit();
    var vm = try run(code, testing.allocator);
    defer vm.deinit();

    std.debug.print("X: {}\n", .{vm.peek()});
    //testing.expectEqual(@as(i64, 10), vm.peek().integer);
}
