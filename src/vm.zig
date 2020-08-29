const std = @import("std");
const compiler = @import("compiler.zig");
const ByteCode = compiler.Compiler.ByteCode;
const byte_code = @import("bytecode.zig");
const _value = @import("value.zig");
const _builtin = @import("builtins.zig");
const Value = _value.Value;
const Type = _value.Type;
const BuiltinError = _builtin.BuiltinError;
const testing = std.testing;
const Allocator = std.mem.Allocator;
const Errors = @import("error.zig").Errors;

//! The Virtual Machine of Luf is stack-based.
//! Currently the stack has a size of 2048 (pre-allocated)

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
    globals: std.ArrayList(*Value),
    /// Call stack that contains the instruction and stack pointer, as well as the instructions
    /// to run the stack
    call_stack: CallStack,
    /// Modules that were imported
    imports: std.StringHashMap(*Value),
    allocator: *Allocator,
    arena: std.heap.ArenaAllocator,
    errors: Errors,

    pub const Error = error{ OutOfMemory, RuntimeError } || BuiltinError;
    const CallStack = std.ArrayList(Frame);
    /// Function `Frame` on the callstack
    const Frame = struct {
        /// Frame pointer which contains the actual function `Value`
        fp: ?*const Value,
        /// `Instruction` pointer to the position of the function in the bytecode's instruction set
        ip: usize,
        /// Stack pointer. Mostly used to reset the stack pointer between scope changes
        sp: usize,
        /// Instructions to run on the current call stack
        instructions: []const byte_code.Instruction,
    };

    pub fn init(allocator: *Allocator) Vm {
        return .{
            .globals = std.ArrayList(*Value).init(allocator),
            .call_stack = CallStack.init(allocator),
            .imports = std.StringHashMap(*Value).init(allocator),
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .errors = Errors.init(allocator),
        };
    }

    /// Frees all memory allocated by the `Vm`.
    /// The vm is no longer valid for use after calling deinit
    pub fn deinit(self: *Vm) void {
        self.globals.deinit();
        self.arena.deinit();
        self.call_stack.deinit();
        self.imports.deinit();
        self.errors.deinit();
        self.* = undefined;
    }

    /// Compiles the given source code and then runs it on the `Vm`
    pub fn compileAndRun(self: *Vm, source: []const u8) !void {
        var code = try compiler.compile(self.allocator, source, &self.errors);
        defer code.deinit();

        try self.run(code);
    }

    /// Runs the given `ByteCode` on the Virtual Machine
    pub fn run(self: *Vm, code: ByteCode) Error!void {
        try self.call_stack.append(.{
            .fp = null,
            .sp = 0,
            .ip = 0,
            .instructions = code.instructions,
        });

        while (self.frame().ip < self.frame().instructions.len) {
            var current_frame = self.frame();
            defer current_frame.ip += 1;
            const inst = current_frame.instructions[current_frame.ip];

            switch (inst.op) {
                .load_const => {
                    const val = try self.newValue();
                    const tmp = code.constants[inst.ptr];
                    val.* = if (tmp.isType(.string))
                        .{ .string = try self.arena.allocator.dupe(u8, tmp.string) }
                    else
                        tmp;
                    try self.push(val);
                },
                .equal,
                .not_equal,
                .less_than,
                .greater_than,
                .less_than_equal,
                .greater_than_equal,
                .@"and",
                .@"or",
                => try self.execCmp(inst.op),
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
                => try self.execBinOp(inst.op),
                .assign_add,
                .assign_div,
                .assign_mul,
                .assign_sub,
                => try self.execAssignAndBinOp(inst.op),
                .pop => _ = self.pop(),
                .load_true => try self.push(&Value.True),
                .load_false => try self.push(&Value.False),
                .minus => try self.execNegation(),
                .not => try self.execNot(),
                .bitwise_not => try self.execBitwiseNot(),
                .load_nil => try self.push(&Value.Nil),
                .load_void => try self.push(&Value.Void),
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
                .make_array => try self.makeArray(inst),
                .make_map => try self.makeMap(inst),
                .get_by_index => try self.getIndexValue(),
                .set_by_index => try self.setIndexValue(),
                .@"return" => {
                    const f = self.call_stack.pop();
                    self.sp = f.sp - 1;

                    try self.push(&Value.Nil);
                },
                .return_value => {
                    const rv = self.pop().?;

                    // remove the function frame from the call stack
                    const f = self.call_stack.pop();
                    self.sp = f.sp - 1;

                    // push the return value back to the stack
                    try self.push(rv);
                },
                .call => try self.execFunctionCall(inst, current_frame.instructions[current_frame.ip + 1]),
                .bind_local => self.stack[current_frame.sp + inst.ptr] = self.pop().?,
                .load_local => try self.push(self.stack[current_frame.sp + inst.ptr]),
                .assign_global => {
                    const value = self.pop().?;
                    if (self.globals.items.len > inst.ptr)
                        self.globals.items[inst.ptr] = value
                    else
                        try self.globals.append(value);
                },
                .assign_local => self.stack[current_frame.sp + inst.ptr] = self.pop().?,
                .load_module => try self.loadModule(),
                .make_iter => try self.makeIterable(inst),
                .iter_next => try self.execNextIter(),
                .make_range => try self.makeRange(),
                .match => try self.execSwitchProng(),
            }
        }

        _ = self.call_stack.popOrNull();
    }

    /// Pushes a new `Value` on top of the `stack` and increases
    /// the stack pointer by 1.
    fn push(self: *Vm, value: *Value) Error!void {
        if (self.sp >= self.stack.len - 1) return self.fail("Stack overflow");

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
    fn execBinOp(self: *Vm, op: byte_code.Opcode) Error!void {
        // right is on the top of left, therefore we pop it first
        const right = self.pop() orelse return self.fail("Missing value");
        const left = self.pop() orelse return self.fail("Missing value");

        const luf_type = try self.resolveType(&[_]*Value{ left, right });

        return switch (luf_type) {
            .integer => self.execIntOp(op, left.integer, right.integer),
            .string => self.execStringOp(op, left.string, right.string),
            else => self.fail("Unexpected type, expected integer or string"),
        };
    }

    /// Analyzes and executes a binary operation on an integer
    fn execIntOp(self: *Vm, op: byte_code.Opcode, left: i64, right: i64) Error!void {
        const result = switch (op) {
            .add => left + right,
            .sub => left - right,
            .mul => left * right,
            .mod => if (right > 0) @mod(left, right) else return self.fail("Rhs can not be a negative integer"),
            .div => blk: {
                if (right == 0) return self.fail("Cannot divide by zero");
                break :blk @divTrunc(left, right);
            },
            .bitwise_and => left & right,
            .bitwise_xor => left ^ right,
            .bitwise_or => left | right,
            .shift_left => blk: {
                if (right < 0) return self.fail("Bit shifting not allowed for negative integers");
                break :blk if (right > std.math.maxInt(u6)) 0 else left << @intCast(u6, right);
            },
            .shift_right => blk: {
                if (right < 0) return self.fail("Bit shifting not allowed for negative integers");
                break :blk if (right > std.math.maxInt(u6)) 0 else left >> @intCast(u6, right);
            },
            else => return self.fail("Unexpected operator"),
        };

        const res = try self.newValue();
        res.* = .{ .integer = result };
        return self.push(res);
    }

    /// Concats two strings together
    fn execStringOp(self: *Vm, op: byte_code.Opcode, left: []const u8, right: []const u8) Error!void {
        if (op != .add) return self.fail("Unexpected operator, expected '+'");

        const res = try self.newValue();
        res.* = .{ .string = try std.mem.concat(&self.arena.allocator, u8, &[_][]const u8{ left, right }) };
        return self.push(res);
    }

    /// Analyzes the instruction, ensures the lhs is an identifier and the rhs is an integer or string.
    /// Strings are only valid for the `assign_add` bytecode instruction
    fn execAssignAndBinOp(self: *Vm, op: byte_code.Opcode) Error!void {
        const right = self.pop() orelse return self.fail("Missing value");
        const left = self.pop() orelse return self.fail("Missing value");

        if (left.lufType() != right.lufType()) return self.fail("Mismatching types");

        if (left.isType(.integer)) {
            try self.execIntOp(switch (op) {
                .assign_add => .add,
                .assign_div => .div,
                .assign_mul => .mul,
                .assign_sub => .sub,
                else => return self.fail("Unexpected operator"),
            }, left.integer, right.integer);
            const val = self.pop().?;
            left.* = val.*;
            return self.push(&Value.Nil);
        }
        if (left.isType(.string)) {
            if (op != .assign_add) return self.fail("Unexpected operator on string, expected '+='");

            try self.execStringOp(.add, left.string, right.string);
            const val = self.pop().?;
            left.* = val.*;
            return self.push(&Value.Nil);
        }

        return self.fail("Unexpected type, expected integer or string");
    }

    /// Analyzes a then executes a comparison and pushes the return value on the stack
    fn execCmp(self: *Vm, op: byte_code.Opcode) Error!void {
        // right is on the top of left, therefore we pop it first
        const right = self.pop() orelse return self.fail("Missing value");
        const left = self.pop() orelse return self.fail("Missing value");

        if (left.lufType() == right.lufType() and left.isType(.integer)) {
            return self.execIntCmp(op, left.integer, right.integer);
        }

        if (left.lufType() == right.lufType() and left.isType(.string)) {
            return self.execStringCmp(op, left.string, right.string);
        }

        const left_bool = left.unwrapAs(.boolean) orelse return self.fail("Expected boolean");
        const right_bool = right.unwrapAs(.boolean) orelse return self.fail("Expected boolean");

        return switch (op) {
            .equal => self.push(if (left_bool == right_bool) &Value.True else &Value.False),
            .not_equal => self.push(if (left_bool != right_bool) &Value.True else &Value.False),
            .@"and" => self.push(if (left_bool and right_bool) &Value.True else &Value.False),
            .@"or" => self.push(if (left_bool or right_bool) &Value.True else &Value.False),
            else => return self.fail("Unexpected operator on boolean"),
        };
    }

    /// Analyzes and compares 2 integers depending on the given operator
    fn execIntCmp(self: *Vm, op: byte_code.Opcode, left: i64, right: i64) Error!void {
        const boolean = switch (op) {
            .equal => left == right,
            .not_equal => left != right,
            .greater_than => left > right,
            .greater_than_equal => left >= right,
            .less_than => left < right,
            .less_than_equal => left <= right,
            else => return self.fail("Unexpected operator"),
        };

        return self.push(if (boolean) &Value.True else &Value.False);
    }

    /// Analyzes and compares 2 strings
    fn execStringCmp(self: *Vm, op: byte_code.Opcode, left: []const u8, right: []const u8) Error!void {
        var eql = std.mem.eql(u8, left, right);
        switch (op) {
            .equal => {},
            .not_equal => eql = !eql,
            else => return self.fail("Unexpected operator, expected '==' or '!='"),
        }

        return self.push(if (eql) &Value.True else &Value.False);
    }

    /// Analyzes and executes a negation
    fn execNegation(self: *Vm) Error!void {
        const right = self.pop() orelse return self.fail("Missing value");
        const integer = right.unwrapAs(.integer) orelse return self.fail("Expected integer");

        const res = try self.newValue();
        res.* = .{ .integer = -integer };
        return self.push(res);
    }

    /// Analyzes and executes the '!' operator
    fn execNot(self: *Vm) Error!void {
        const right = self.pop() orelse return self.fail("Missing value");

        const val = switch (right.*) {
            .boolean => !right.boolean,
            .nil => true,
            else => false,
        };
        return self.push(if (val) &Value.True else &Value.False);
    }

    /// Executes the ~ operator
    fn execBitwiseNot(self: *Vm) Error!void {
        const value = self.pop() orelse return self.fail("Missing value");

        const integer = value.unwrapAs(.integer) orelse return self.fail("Expected integer");

        const ret = try self.newValue();
        ret.* = .{ .integer = ~integer };
        return self.push(ret);
    }

    /// Analyzes the instruction and builds an array
    fn makeArray(self: *Vm, inst: byte_code.Instruction) Error!void {
        const len = inst.ptr;
        var list = Value.List{};
        try list.resize(&self.arena.allocator, len);
        errdefer list.deinit(&self.arena.allocator);

        const res = try self.newValue();
        if (len == 0) {
            res.* = .{ .list = list };
            return self.push(res);
        }

        var list_type: Type = undefined;
        var i: usize = 1;

        while (i <= len) : ({
            i += 1;
        }) {
            const val = self.pop().?;

            if (i == 1)
                list_type = val.lufType()
            else if (!val.isType(list_type)) return self.fail("Mismatching types");
            list.items[len - i] = val;
        }

        res.* = .{ .list = list };
        return self.push(res);
    }

    /// Analyzes and creates a new map
    fn makeMap(self: *Vm, inst: byte_code.Instruction) Error!void {
        const len = inst.ptr / 2;
        var map = Value.Map{};
        errdefer map.deinit(&self.arena.allocator);

        const res = try self.newValue();
        if (len == 0) {
            res.* = .{ .map = map };
            return self.push(res);
        }

        try map.ensureCapacity(&self.arena.allocator, len);

        var key_type: Type = undefined;
        var value_type: Type = undefined;
        var i: usize = 0;

        while (i < len) : ({
            i += 1;
        }) {
            const value = self.pop().?;
            const key = self.pop().?;

            if (i == 0) {
                key_type = key.lufType();
                value_type = value.lufType();
            } else {
                if (!key.isType(key_type)) return self.fail("Mismatching types");
                if (!value.isType(value_type)) return self.fail("Mismatching types");
            }

            map.putAssumeCapacity(key, value);
        }
        res.* = .{ .map = map };
        return self.push(res);
    }

    /// Creates a new iterable
    fn makeIterable(self: *Vm, inst: byte_code.Instruction) Error!void {
        const iterable = self.pop() orelse return self.fail("Missing value");

        switch (iterable.lufType()) {
            .list, .range, .string => {},
            else => return self.fail("Unsupported value for iterable"),
        }

        const value = try self.newValue();
        value.* = .{
            .iterable = .{
                .expose_index = inst.ptr != 0,
                .index = 0,
                .value = iterable,
            },
        };

        self.sp += 2;

        return self.push(value);
    }

    /// Pops the iterator from the stack and retrieves the next value
    /// Pushes the iterator back, then the value and then the index if exposed
    /// Finally, a true or false is pushed to determine if we should end the for loop
    fn execNextIter(self: *Vm) Error!void {
        const value = self.pop().?;

        var iterator = value.unwrapAs(.iterable) orelse return self.fail("Expected iterable");
        const next = try self.newValue();
        try iterator.next(&self.arena.allocator, next);
        if (!next.isType(.nil)) {
            // push the iterator back on the stack
            value.iterable = iterator;
            try self.push(value);

            // push the index if it is exposed
            if (iterator.expose_index) {
                const index = try self.newValue();
                index.* = .{ .integer = @intCast(i64, iterator.index - 1) };
                try self.push(index);
            }
            // push the capture on the stack
            try self.push(next);

            // push true to continue
            return self.push(&Value.True);
        } else {
            return self.push(&Value.False);
        }
    }

    /// Creates a range from 2 values
    /// Returns an error if lhs or rhs is not an integer
    fn makeRange(self: *Vm) Error!void {
        const right = self.pop().?;
        const left = self.pop().?;

        const ret = try self.newValue();
        ret.* = .{
            .range = .{
                .start = left.unwrapAs(.integer) orelse return self.fail("Expected integer for range"),
                .end = right.unwrapAs(.integer) orelse return self.fail("Expected integer for range"),
            },
        };
        return self.push(ret);
    }

    /// Analyzes an index/reference pushes the value on the stack
    fn getIndexValue(self: *Vm) Error!void {
        const index = self.pop() orelse return self.fail("Missing value");
        const left = self.pop() orelse return self.fail("Missing value");

        switch (left.*) {
            .list => |list| {
                switch (index.*) {
                    .integer => |int| {
                        if (int < 0 or int > list.items.len) return self.fail("Out of bounds");
                        return self.push(list.items[@intCast(u64, int)]);
                    },
                    .string => |name| {
                        if (_builtin.builtins.get(name)) |val| {
                            const builtin = val.native;
                            const args = try self.allocator.alloc(*Value, builtin.arg_len + 1);
                            defer self.allocator.free(args);
                            args[0] = left;
                            var i: usize = 1;

                            while (i <= builtin.arg_len) : (i += 1) args[i] = self.pop().?;

                            //create a shallow copy
                            const res = try self.newValue();
                            res.* = (builtin.func(
                                self,
                                args,
                            ) catch return self.fail("Could not execute builtin function")).*;

                            return self.push(res);
                        }
                    },
                    else => return self.fail("Expected string or integer on rhs"),
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
                        if (int < 0 or int > string.len) return self.fail("Out of bounds");

                        const val = try self.newValue();
                        val.* = .{ .string = string[@intCast(usize, int)..@intCast(usize, int + 1)] };
                        return self.push(val);
                    },
                    .string => |name| {
                        if (_builtin.builtins.get(name)) |val| {
                            const builtin = val.native;

                            //create a shallow copy
                            const res = try self.newValue();
                            res.* = (builtin.func(
                                self,
                                &[_]*Value{left},
                            ) catch return self.fail("Could not execute builtin function")).*;

                            return self.push(res);
                        }
                    },
                    else => return self.fail("Expected string or integer on rhs"),
                }
            },
            .module => |mod| {
                if (!index.isType(.string)) return self.fail("Expected string");

                // Find the definition in the module, if it doesn't exist, it's an error.
                // this differs from a regular map where we return null
                if (mod.attributes.map.get(index)) |val| {
                    return self.push(val);
                } else {
                    return self.fail("Identifier not found in module");
                }
            },
            ._enum => |enm| {
                const enum_value = index.unwrapAs(.string) orelse return self.fail("Expected string");

                for (enm) |field, i| {
                    if (std.mem.eql(u8, field, enum_value)) {
                        const ret = try self.newValue();
                        ret.* = .{ .integer = @intCast(i64, i) };
                        return self.push(ret);
                    }
                }

                return self.fail("Enum identifier does not exist");
            },
            else => return self.fail("Unsupported type"),
        }
    }

    /// Sets the lhs by the value on top of the current stack
    /// This also does type checking to ensure the lhs and rhs are equal types
    fn setIndexValue(self: *Vm) Error!void {
        const value = self.pop() orelse return self.fail("Missing value");
        const right = self.pop() orelse return self.fail("Missing value");
        const left = self.pop() orelse return self.fail("Missing value");

        if (left.isType(.list) and right.isType(.integer)) {
            const list = left.list;
            if (right.integer < 0 or right.integer > list.items.len) return self.fail("Out of bounds");
            list.items[@intCast(usize, right.integer)].* = value.*;
            return self.push(&Value.Nil);
        } else if (left.isType(.map)) {
            const map = left.map;
            if (map.get(right)) |val| {
                val.* = value.*;
                return self.push(&Value.Nil);
            } else {
                // replace with more descriptive Error
                return self.fail("Value not found");
            }
        }

        // replace with more descriptive Error
        return self.fail("Unsupported type");
    }

    /// Analyzes the current instruction to execture a function call
    /// Expects the current instruction pointer.
    /// This will also return the new instruction pointer
    ///
    /// `next` is required to detect if we can optimize tail recursion
    fn execFunctionCall(self: *Vm, inst: byte_code.Instruction, next: byte_code.Instruction) Error!void {
        const arg_len = inst.ptr;
        const val = self.stack[self.sp - (1 + arg_len)];

        // if it's not a function call, we can expect it to be a builtin function
        if (val.* != .function) return;

        if (arg_len != val.function.arg_len) return self.fail("Mismatching argument length");

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

    /// Reads the file name, open it, and compile its source code.
    /// After compilator, the symbols will be saved in a map and returned
    fn importModule(self: *Vm, file_name: []const u8) Error!*Value {
        const file = std.fs.cwd().openFile(file_name, .{}) catch return Error.RuntimeError;

        defer file.close();
        const size = file.getEndPos() catch return Error.RuntimeError;

        const source = file.readAllAlloc(self.allocator, size, size) catch return Error.RuntimeError;
        defer self.allocator.free(source);

        // we should probably save this bytecode so we can free it at a later point
        var code = compiler.compile(self.allocator, source, &self.errors) catch return Error.RuntimeError;
        defer code.deinit();

        const last_ip = self.ip;
        const last_sp = self.sp;

        try self.run(code);

        // Get all constants exposed by the source file
        var attributes = Value.Map{};
        try attributes.ensureCapacity(&self.arena.allocator, code.symbols.items().len);
        for (code.symbols.items()) |entry, i| {
            const symbol: compiler.Compiler.Symbol = entry.value;
            std.debug.assert(symbol.scope == .global);

            const name = try self.newValue();
            name.* = .{ .string = try self.arena.allocator.dupe(u8, symbol.name) };
            const value = try self.newValue();
            value.* = code.constants[symbol.index];

            // Create a copy of the function instructions as we're free'ing all compiler memory on scope exit
            if (value.isType(.function)) {
                value.function.instructions = try self.arena.allocator.dupe(byte_code.Instruction, value.function.instructions);
            }

            if (value.isType(._enum)) {
                value._enum = try self.arena.allocator.dupe([]const u8, value._enum);
            }

            attributes.putAssumeCapacityNoClobber(name, value);
        }

        const ret = try self.newValue();
        ret.* = .{ .map = attributes };
        return ret;
    }

    /// Loads a module, if not existing, compiles the source code of the given file name
    fn loadModule(self: *Vm) Error!void {
        const val = self.pop().?;
        const name = val.unwrapAs(.string) orelse return self.fail("Expected a string");

        const mod = self.imports.get(name) orelse blk: {
            const imp = self.importModule(name) catch return self.fail("Could not import module");
            try self.imports.put(name, imp);
            break :blk imp;
        };

        const ret = try self.newValue();
        ret.* = .{
            .module = .{
                .name = name,
                .attributes = mod,
            },
        };

        return self.push(ret);
    }

    /// Compares switch' capture and the current prong.
    /// Unlike execCmp, this does not pop the lhs value from the stack, to maintain it's position
    /// as it's used for the other prongs. it will be popped at the end of the switch statement.
    fn execSwitchProng(self: *Vm) Error!void {
        const prong_value = self.pop().?;
        const capture_value = self.stack[self.sp - 1];

        switch (prong_value.*) {
            .integer => |integer| {
                const capture = capture_value.unwrapAs(.integer) orelse return self.fail("Expected an integer");
                return self.execIntCmp(.equal, capture, integer);
            },
            .string => |string| {
                const capture = capture_value.unwrapAs(.string) orelse return self.fail("Expected a string");
                return self.execStringCmp(.equal, capture, string);
            },
            .range => |range| {
                const capture = capture_value.unwrapAs(.integer) orelse return self.fail("Expected an integer");
                return self.push(if (capture >= range.start and capture <= range.end) &Value.True else &Value.False);
            },
            else => return self.fail("Unsupported type, switching are only allowed for integers, strings and ranges"),
        }
    }

    /// Creates a new Value on the heap which will be freed on scope exits (call_stack pops)
    fn newValue(self: *Vm) !*Value {
        return self.arena.allocator.create(Value);
    }

    /// Appends an error message to `errors` and returns a `Vm.Error`
    fn fail(self: *Vm, comptime msg: []const u8) Error!void {
        //TODO implement a way to add location information
        try self.errors.add("Runtime error: " ++ msg, 0, .err);
        return Error.RuntimeError;
    }

    /// Checks each given type if they are equal or not
    fn resolveType(self: *Vm, values: []*const Value) Error!Type {
        std.debug.assert(values.len > 0);
        const cur_tag: Type = values[0].lufType();
        if (values.len == 1) return cur_tag;

        for (values[1..]) |value|
            if (!value.isType(cur_tag)) try self.fail("Mismatching types");

        return cur_tag;
    }
};

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
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        testing.expect(case.expected == vm.peek().integer);
        testing.expectEqual(@as(usize, 0), vm.sp);
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
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        testing.expect(case.expected == vm.peek().boolean);
        testing.expectEqual(@as(usize, 0), vm.sp);
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
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        if (@TypeOf(case.expected) == comptime_int) {
            testing.expect(case.expected == vm.peek().integer);
        } else {
            testing.expect(vm.peek().* == ._void);
        }
        testing.expectEqual(@as(usize, 0), vm.sp);
    }
}

test "Declaration" {
    const test_cases = .{
        .{ .input = "const x = 1 x", .expected = 1 },
        .{ .input = "const x = 1 const y = 1 x + y", .expected = 2 },
        .{ .input = "mut x = 1 const y = x + x x + y", .expected = 3 },
    };

    inline for (test_cases) |case| {
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        testing.expect(case.expected == vm.peek().integer);
        testing.expectEqual(@as(usize, 0), vm.sp);
    }
}

test "Strings" {
    const test_cases = .{
        .{ .input = "\"foo\"", .expected = "foo" },
        .{ .input = "\"foo\" + \"bar\"", .expected = "foobar" },
        .{ .input = "const x = \"foo\" x+=\"bar\" x", .expected = "foobar" },
    };

    inline for (test_cases) |case| {
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        testing.expectEqualStrings(case.expected, vm.peek().string);
        testing.expectEqual(@as(usize, 0), vm.sp);
    }
}

test "Arrays" {
    const test_cases = .{
        .{ .input = "[1, 2, 3]", .expected = &[_]i64{ 1, 2, 3 } },
        .{ .input = "[]", .expected = &[_]i64{} },
        .{ .input = "[1 + 1, 2 * 2, 6]", .expected = &[_]i64{ 2, 4, 6 } },
    };

    inline for (test_cases) |case| {
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        const list = vm.peek().list;
        testing.expect(list.items.len == case.expected.len);
        inline for (case.expected) |int, i| {
            const items = list.items;
            testing.expectEqual(int, items[i].integer);
        }
        testing.expectEqual(@as(usize, 0), vm.sp);
    }
}

test "Maps" {
    const test_cases = .{
        .{ .input = "{1:2, 2:1, 5:6}", .expected = &[_]i64{ 6, 1, 2 }, .keys = &[_]i64{ 5, 2, 1 } },
        .{ .input = "{}", .expected = &[_]i64{}, .keys = &[_]i64{} },
        .{ .input = "{\"foo\":1}", .expected = &[_]i64{1}, .keys = &[_][]const u8{"foo"} },
    };

    inline for (test_cases) |case| {
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
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
        testing.expectEqual(@as(usize, 0), vm.sp);
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
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        if (@TypeOf(case.expected) == comptime_int)
            testing.expectEqual(@as(i64, case.expected), vm.peek().integer)
        else if (@TypeOf(case.expected) == *const [1:0]u8)
            testing.expectEqualStrings(case.expected, vm.peek().string)
        else
            testing.expectEqual(case.expected, vm.peek());
        testing.expectEqual(@as(usize, 0), vm.sp);
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
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        if (@TypeOf(case.expected) == comptime_int)
            testing.expectEqual(@as(i64, case.expected), vm.peek().integer)
        else
            testing.expectEqual(case.expected, vm.peek());
        testing.expectEqual(@as(usize, 0), vm.sp);
    }
}

test "Globals vs Locals" {
    const test_cases = .{
        .{ .input = "const x = fn() { const x = 5 return x } x()", .expected = 5 },
        .{ .input = "const x = fn() { const y = 1 const z = 2 return y + z } x()", .expected = 3 },
    };

    inline for (test_cases) |case| {
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        if (@TypeOf(case.expected) == comptime_int)
            testing.expectEqual(@as(i64, case.expected), vm.peek().integer)
        else
            testing.expectEqual(case.expected, vm.peek().*);
        testing.expectEqual(@as(usize, 0), vm.sp);
    }
}

test "Functions with arguments" {
    const test_cases = .{
        .{ .input = "const x = fn(x) { return x } x(3)", .expected = 3 },
        .{ .input = "const x = fn(a, b) { return a + b } x(3,5)", .expected = 8 },
        .{ .input = "const x = fn(a, b) { const z = a + b return z } x(3,5)", .expected = 8 },
    };

    inline for (test_cases) |case| {
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        if (@TypeOf(case.expected) == comptime_int)
            testing.expectEqual(@as(i64, case.expected), vm.peek().integer)
        else
            testing.expectEqual(case.expected, vm.peek());
        testing.expectEqual(@as(usize, 0), vm.sp);
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
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        testing.expectEqual(@as(i64, case.expected), vm.peek().integer);
        testing.expectEqual(@as(usize, 0), vm.sp);
    }
}

test "While loop" {
    const test_cases = .{
        .{ .input = "mut i = 0 while (i > 10) {mut i = 10} i", .expected = 0 },
        .{ .input = "mut i = 0 while (i < 10) {i = 10} i", .expected = 10 },
        .{ .input = "mut i = 0 while (i < 10) { if(i==5) { break } i = 5} i", .expected = 5 },
    };

    inline for (test_cases) |case| {
        var vm = Vm.init(testing.allocator);
        try vm.compileAndRun(case.input);
        defer vm.deinit();

        testing.expectEqual(@as(i64, case.expected), vm.peek().integer);
        testing.expectEqual(@as(usize, 0), vm.sp);
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
    var vm = Vm.init(testing.allocator);
    try vm.compileAndRun(input);
    defer vm.deinit();

    testing.expectEqual(@as(i64, 10), vm.peek().integer);
    testing.expectEqual(@as(usize, 0), vm.sp);
}

test "For loop" {
    const input =
        \\mut sum = 0
        \\for([1, 3, 5, 7, 9]) |item, i| {
        \\  if (item == 3) {
        \\      continue
        \\  }
        \\  if (item == 7) {
        \\      break
        \\  }
        \\  sum += item + i
        \\}
        \\sum
    ;
    var vm = Vm.init(testing.allocator);
    try vm.compileAndRun(input);
    defer vm.deinit();

    testing.expectEqual(@as(i64, 8), vm.peek().integer);
    testing.expectEqual(@as(usize, 0), vm.sp);
}

test "Range" {
    const input =
        \\mut sum = 0
        \\for(1..100) |e, i| {
        \\  if (e % 2 == 0) {
        \\      continue
        \\  }
        \\  sum += e + i
        \\}
        \\sum
    ;
    var vm = Vm.init(testing.allocator);
    try vm.compileAndRun(input);
    defer vm.deinit();

    testing.expectEqual(@as(i64, 4950), vm.peek().integer);
    testing.expectEqual(@as(usize, 0), vm.sp);
}

test "For loop - String" {
    const input = "mut result = \"hello\" const string = \"world\" for(string)|c, i|{result+=c}result";
    var vm = Vm.init(testing.allocator);
    try vm.compileAndRun(input);
    defer vm.deinit();

    testing.expectEqualStrings("helloworld", vm.peek().string);
    testing.expectEqual(@as(usize, 0), vm.sp);
}

test "Enum expression and comparison" {
    const input =
        \\const x = enum{value, another_value}
        \\const enum_value = x.another_value
        \\if (enum_value == x.another_value) {
        \\  5
        \\}
    ;
    var vm = Vm.init(testing.allocator);
    try vm.compileAndRun(input);
    defer vm.deinit();

    testing.expectEqual(@as(i64, 5), vm.peek().integer);
    testing.expectEqual(@as(usize, 0), vm.sp);
}

test "Enum expression and comparison" {
    const input =
        \\const range = 0..9
        \\mut x = 0
        \\switch(5) {
        \\  4: x += 10,
        \\  range: x += 30,
        \\  5: x += 20
        \\}
        \\x
    ;
    var vm = Vm.init(testing.allocator);
    try vm.compileAndRun(input);
    defer vm.deinit();
    testing.expectEqual(@as(i64, 50), vm.peek().integer);
    testing.expectEqual(@as(usize, 0), vm.sp);
}
