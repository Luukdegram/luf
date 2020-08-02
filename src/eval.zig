const Node = @import("ast.zig").Node;
const Value = @import("value.zig").Value;
const Type = @import("value.zig").Type;
const Scope = @import("value.zig").Scope;
const Lexer = @import("lexer.zig").Lexer;
const parser = @import("parser.zig");
const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;

//! This contains all the functions required to evaluate
//! a set of nodes into one or more Luf values

/// Possible errors that can be triggered when evaluation one or multiple nodes
pub const EvalError = error{
    UnsupportedOperator,
    TypeMismatch,
    UnknownIdentifier,
    OutOfBounds,
    MemoryNotExists,
    OutOfMemory,
    UnsupportedType,
    MemberDoesNotExist,
    MismatchingTypes,
};

/// Evaluates the given node and returns a Value
pub fn eval(node: Node, scope: *Scope) EvalError!Value {
    return switch (node) {
        .expression => |exp| eval(exp.value, scope),
        .int_lit => |lit| Value{ .integer = @bitCast(i64, lit.value) },
        .boolean => |bl| Value{ .boolean = bl.value },
        .prefix => |pfx| evalPrefix(pfx, scope),
        .block_statement => |block| evalBlock(block, scope),
        .if_expression => |if_exp| evalIf(if_exp, scope),
        .infix => |ifx| evalInfix(ifx, scope),
        ._return => |ret| Value{ ._return = &(try eval(ret.value, scope)) },
        .identifier => |id| evalIdentifier(id, scope),
        .func_lit => |func| Value{
            .function = .{
                .params = func.params,
                .body = func.body,
                .scope = scope,
            },
        },
        .declaration => |decl| {
            const val = try eval(decl.value, scope);
            try scope.set(decl.name.identifier.value, val);
            return val;
        },
        .call_expression => |call| {
            const func = try eval(call.function, scope);

            const args = try evalArguments(call.arguments, scope);
            return callFunction(func, args);
        },
        .string_lit => |string| Value{ .string = string.value },
        .array => |array| {
            const elements = try evalArguments(array.value, scope);
            try assertSameTypes(elements);
            var list = Value.List.fromOwnedSlice(scope.allocator, elements);
            return Value{ .list = list };
        },
        .map => |map| evalMap(map, scope),
        .index => |index| evalIndex(index, scope),
        else => {
            @import("std").debug.panic("TODO: Implement node: {}\n", .{@tagName(node)});
        },
    };
}

/// Evaluates the nodes and returns the final Value
pub fn evalNodes(nodes: []const Node, scope: *Scope) EvalError!Value {
    var value: Value = undefined;
    for (nodes) |node| {
        value = try eval(node, scope);

        if (value == ._return) {
            return value._return.*;
        }
    }

    return value;
}

fn evalArguments(nodes: []const Node, scope: *Scope) EvalError![]Value {
    var values = std.ArrayList(Value).init(scope.allocator);
    for (nodes) |node| {
        const res = try eval(node, scope);
        try values.append(res);
    }
    return values.toOwnedSlice();
}

fn callFunction(func: Value, args: []Value) EvalError!Value {
    // create new scope
    var scope = try func.function.scope.clone();
    for (func.function.params) |param, i| {
        try scope.set(param.identifier.value, args[i]);
    }

    const return_val = try eval(func.function.body, scope);
    if (return_val == ._return) {
        return return_val._return.*;
    }
    return return_val;
}

fn evalBlock(block: *const Node.BlockStatement, scope: *Scope) EvalError!Value {
    var value: Value = undefined;
    for (block.nodes) |node| {
        value = try eval(node, scope);

        if (value == ._return and value._return.* != .nil) {
            return value;
        }
    }

    return value;
}

fn evalPrefix(prefix: *const Node.Prefix, scope: *Scope) EvalError!Value {
    return switch (prefix.operator) {
        .bang => evalBangOperator(try eval(prefix.right, scope)),
        .minus => evalNegation(try eval(prefix.right, scope)),
    };
}

fn evalInfix(infix: *const Node.Infix, scope: *Scope) !Value {
    const left = try eval(infix.left, scope);
    const right = try eval(infix.right, scope);

    return switch (resolveType(left, right)) {
        .integer => evalIntegerInfix(infix.operator, left, right),
        .string => evalStringInfix(scope.allocator, infix.operator, left, right),
        .boolean => switch (infix.operator) {
            .equal => Value{ .boolean = left.boolean == right.boolean },
            .not_equal => Value{ .boolean = left.boolean != right.boolean },
            else => Value{ .nil = {} },
        },
        else => Value{ .nil = {} },
    };
}

/// Determines the type based on the given values
/// Returns the Type nil if the types are unequal
fn resolveType(left: Value, right: Value) Type {
    var current: Type = std.meta.activeTag(left);
    return if (current != std.meta.activeTag(right))
        .nil
    else
        current;
}

fn evalIntegerInfix(operator: Node.Infix.Op, left: Value, right: Value) Value {
    const left_val = left.integer;
    const right_val = right.integer;

    return switch (operator) {
        .add => Value{ .integer = @bitCast(i64, left_val + right_val) },
        .sub => Value{ .integer = @bitCast(i64, left_val) - @bitCast(i64, right_val) },
        .multiply => Value{ .integer = @bitCast(i64, left_val * right_val) },
        .divide => Value{ .integer = @divTrunc(@bitCast(i64, left_val), @bitCast(i64, right_val)) },
        .less_than => Value{ .boolean = left_val < right_val },
        .greater_than => Value{ .boolean = left_val > right_val },
        .equal => Value{ .boolean = left_val == right_val },
        .not_equal => Value{ .boolean = left_val != right_val },
        else => Value{ .nil = {} },
    };
}

fn evalStringInfix(allocator: *std.mem.Allocator, operator: Node.Infix.Op, left: Value, right: Value) EvalError!Value {
    if (operator != .add) {
        return EvalError.UnsupportedOperator;
    }
    const left_val = left.string;
    const right_val = right.string;

    var result = try std.mem.concat(allocator, u8, &[_][]const u8{
        left_val,
        right_val,
    });

    return Value{ .string = result };
}

fn evalBangOperator(right: Value) Value {
    return switch (right) {
        .boolean => |val| Value{ .boolean = !val },
        .nil => Value{ .boolean = true },
        else => Value{ .boolean = false },
    };
}

fn evalNegation(right: Value) Value {
    assert(right == .integer);
    return Value{ .integer = -right.integer };
}

fn evalIdentifier(identifier: *const Node.Identifier, scope: *Scope) EvalError!Value {
    if (scope.get(identifier.value)) |val| {
        return val;
    } else {
        return EvalError.UnknownIdentifier;
    }
}

fn evalIf(exp: *const Node.IfExpression, scope: *Scope) EvalError!Value {
    const condition = try eval(exp.condition, scope);

    if (isTrue(condition)) {
        return eval(exp.true_pong, scope);
    } else if (exp.false_pong) |pong| {
        return eval(pong, scope);
    } else {
        return Value{ .nil = {} };
    }
}

/// Evaluates a value and determines if the value is considered true or false
fn isTrue(value: Value) bool {
    return switch (value) {
        .nil => false,
        .boolean => |val| val,
        else => true,
    };
}

/// Evaluates the index node and retrieves the value at the parsed index of the
/// array or map.
fn evalIndex(node: *const Node.IndexExpression, scope: *Scope) EvalError!Value {
    var left = try eval(node.left, scope);

    // check if index is a builtin
    if (node.index == .call_expression) {
        const call: *Node.CallExpression = node.index.call_expression;
        var args = try evalArguments(call.arguments, scope);

        if (Value.builtins.get(call.function.identifier.value)) |builtin| {
            var func_args = try scope.allocator.alloc(Value, args.len + 1);
            defer scope.allocator.free(func_args);

            func_args[0] = left;
            std.mem.copy(Value, func_args[1..], args);
            const result = (try builtin.builtin.func(func_args)).*;
            try scope.set(node.left.identifier.value, result);
            return result;
        }
    } else if (node.index == .identifier) {
        if (Value.builtins.get(node.index.identifier.value)) |id| {
            const result = try id.builtin.func(&[_]Value{left});
            return result.*;
        }
    }
    var index: Value = try eval(node.index, scope);

    if (left == .list and index == .integer) {
        const pos = index.integer;
        const list = left.list;

        if (pos >= list.items.len) {
            return EvalError.OutOfBounds;
        }
        return list.items[@intCast(usize, pos)];
    } else if (left == .map) {
        const map: Value.Map = left.map;
        if (map.get(index)) |val| {
            return val;
        }
        return Value{ .nil = {} };
    } else {
        return EvalError.UnsupportedOperator;
    }
}

/// Evaluates a map literal and its internals. Also ensures the internal
/// keys and values have the same type
fn evalMap(node: *const Node.MapLiteral, scope: *Scope) EvalError!Value {
    var map = Value.Map.init(scope.allocator);
    var key_type: Value = undefined;

    if (node.value.len == 0) return Value{ .map = map };

    const first_pair = node.value[0].map_pair;
    const key = try eval(first_pair.key, scope);
    const value = try eval(first_pair.value, scope);

    try map.put(key, value);

    if (node.value.len == 1) return Value{ .map = map };

    for (node.value[1..]) |n| {
        const pair = n.map_pair;
        const k = try eval(pair.key, scope);
        const v = try eval(pair.value, scope);

        if (std.meta.activeTag(key) != std.meta.activeTag(k)) {
            return EvalError.MismatchingTypes;
        }

        if (std.meta.activeTag(value) != std.meta.activeTag(v)) {
            return EvalError.MismatchingTypes;
        }

        try map.put(k, v);
    }

    return Value{ .map = map };
}

fn assertSameTypes(values: []Value) EvalError!void {
    if (values.len < 2) return;
    var current = std.meta.activeTag(values[0]);
    for (values[1..]) |val| {
        if (current != std.meta.activeTag(val)) {
            return EvalError.MismatchingTypes;
        }
    }
}

test "Eval integer" {
    const test_cases = .{
        .{ .input = "20", .expected = @as(i64, 20) },
        .{ .input = "100", .expected = @as(i64, 100) },
        .{ .input = "-20", .expected = @as(i64, -20) },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = @as(i64, 10) },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = @as(i64, 32) },
        .{ .input = "-50 + 100 + -50", .expected = @as(i64, 0) },
        .{ .input = "5 * 2 + 10", .expected = @as(i64, 20) },
        .{ .input = "5 + 2 * 10", .expected = @as(i64, 25) },
        .{ .input = "20 + 2 * -10", .expected = @as(i64, 0) },
        .{ .input = "50 / 2 * 2 + 10", .expected = @as(i64, 60) },
        .{ .input = "2 * (5 + 10)", .expected = @as(i64, 30) },
        .{ .input = "3 * 3 * 3 + 10", .expected = @as(i64, 37) },
        .{ .input = "3 * (3 * 3) + 10", .expected = @as(i64, 37) },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = @as(i64, 50) },
    };

    inline for (test_cases) |case| {
        const tree = try parser.parse(testing.allocator, case.input);
        defer tree.deinit();

        var scope = Scope.init(testing.allocator);
        defer scope.deinit();

        const value = try evalNodes(tree.nodes, &scope);
        testing.expectEqual(case.expected, value.integer);
    }
}

test "Eval boolean" {
    const test_cases = .{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
    };

    inline for (test_cases) |case| {
        const tree = try parser.parse(testing.allocator, case.input);
        defer tree.deinit();

        var scope = Scope.init(testing.allocator);
        defer scope.deinit();

        const value = try evalNodes(tree.nodes, &scope);
        testing.expectEqual(case.expected, value.boolean);
    }
}

test "Eval bang" {
    const test_cases = .{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!5", .expected = false },
        .{ .input = "!!true", .expected = true },
    };

    inline for (test_cases) |case| {
        const tree = try parser.parse(testing.allocator, case.input);
        defer tree.deinit();

        var scope = Scope.init(testing.allocator);
        defer scope.deinit();

        const value = try evalNodes(tree.nodes, &scope);
        testing.expectEqual(case.expected, value.boolean);
    }
}

test "Eval if else" {
    const test_cases = .{
        .{ .input = "if (true) { 10 }", .expected = 10 },
        .{ .input = "if (false) { 10 }", .expected = {} },
        .{ .input = "if(1) { 10 }", .expected = 10 },
        .{ .input = "if(1 < 2) { 10 }", .expected = 10 },
        .{ .input = "if(1 > 2) { 10 }", .expected = {} },
        .{ .input = "if (1 > 2) {10 } else { 20 }", .expected = 20 },
        .{ .input = "if (1 < 2) {10 } else { 20 }", .expected = 10 },
    };

    inline for (test_cases) |case| {
        const tree = try parser.parse(testing.allocator, case.input);
        defer tree.deinit();

        var scope = Scope.init(testing.allocator);
        defer scope.deinit();

        const value = try evalNodes(tree.nodes, &scope);
        switch (@typeInfo(@TypeOf(case.expected))) {
            .ComptimeInt => testing.expectEqual(@intCast(i64, case.expected), value.integer),
            else => testing.expect(value == .nil),
        }
    }
}

test "Eval return value" {
    const test_cases = .{
        .{ .input = "return 10", .expected = 10 },
        .{ .input = "return 10 9", .expected = 10 },
        .{ .input = "return 2 * 5 9", .expected = 10 },
        .{ .input = "9 return 2 * 5 9", .expected = 10 },
        .{
            .input =
                \\if(10 > 1) {
                \\  if (10 > 1) {
                \\      return 10
                \\  }
                \\  
                \\  return 1
                \\}
            ,
            .expected = 10,
        },
    };

    inline for (test_cases) |case| {
        const tree = try parser.parse(testing.allocator, case.input);
        defer tree.deinit();

        var scope = Scope.init(testing.allocator);
        defer scope.deinit();

        const value = try evalNodes(tree.nodes, &scope);
        testing.expectEqual(@intCast(i64, case.expected), value.integer);
    }
}

test "Eval declaration" {
    const test_cases = .{
        .{ .input = "const a = 5 a", .expected = 5 },
        .{ .input = "mut a = 5 * 5 a", .expected = 25 },
        .{ .input = "const a = 5 const b = a b", .expected = 5 },
        .{ .input = "mut a = 5 const b = a mut c = a + b + 5 c", .expected = 15 },
    };

    inline for (test_cases) |case| {
        const tree = try parser.parse(testing.allocator, case.input);
        defer tree.deinit();

        var scope = Scope.init(testing.allocator);
        defer scope.deinit();

        const value = try evalNodes(tree.nodes, &scope);
        testing.expectEqual(@intCast(i64, case.expected), value.integer);
    }
}

test "Basic function" {
    const input = "fn(x) { x + 5 }";
    const tree = try parser.parse(testing.allocator, input);
    defer tree.deinit();
    var scope = Scope.init(testing.allocator);
    defer scope.deinit();

    const res = try evalNodes(tree.nodes, &scope);
    testing.expect(res == .function);
    testing.expect(res.function.params.len == 1);
    testing.expectEqualSlices(u8, "x", res.function.params[0].identifier.value);
}

test "Function calling" {
    const test_cases = .{
        .{ .input = "const my_func = fn(x){x} my_func(5)", .expected = 5 },
        .{ .input = "const my_func = fn(x){return x} my_func(5)", .expected = 5 },
        .{ .input = "const double = fn(x){ x * 2 } double(5)", .expected = 10 },
        .{ .input = "const add = fn(x, y){ x + y } add(5, 5)", .expected = 10 },
        .{ .input = "const add = fn(x, y){ x + y } add(5 + 5, add(5, 5))", .expected = 20 },
        .{ .input = "fn(x){x}(5)", .expected = 5 },
    };

    inline for (test_cases) |case| {
        //wrap in an arena for now until we fix memory leak in function arguments
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const tree = try parser.parse(&arena.allocator, case.input);
        defer tree.deinit();

        var scope = Scope.init(&arena.allocator);
        defer scope.deinit();

        const value = try evalNodes(tree.nodes, &scope);
        testing.expectEqual(@intCast(i64, case.expected), value.integer);
    }
}

test "Closure" {
    const input =
        \\const my_func = fn(x){
        \\  fn(y){ x + y }        
        \\}
        \\const other_func = my_func(2)
        \\other_func(2)
    ;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const tree = try parser.parse(&arena.allocator, input);
    defer tree.deinit();

    var scope = Scope.init(&arena.allocator);
    defer scope.deinit();

    const value = try evalNodes(tree.nodes, &scope);
    testing.expectEqual(@intCast(i64, 4), value.integer);
}

test "String literal" {
    const input = "\"Some test input\"";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const tree = try parser.parse(&arena.allocator, input);
    defer tree.deinit();

    var scope = Scope.init(&arena.allocator);
    defer scope.deinit();

    const value = try evalNodes(tree.nodes, &scope);
    testing.expectEqualSlices(u8, "Some test input", value.string);
}

test "String concatenation" {
    const input = "\"Hello\" + \" \" + \"world\"";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const tree = try parser.parse(&arena.allocator, input);
    defer tree.deinit();

    var scope = Scope.init(&arena.allocator);
    defer scope.deinit();

    const value = try evalNodes(tree.nodes, &scope);
    testing.expectEqualSlices(u8, "Hello world", value.string);
}

test "Builtins" {
    const test_cases = .{
        .{ .input = "const len = \"test\".len const x = len + 1", .expected = 5 },
        .{ .input = "const array = [1,2,3] const x = array.len", .expected = 3 },
        .{ .input = "mut array = [1, 2, 3] array.add(1) const x = array.len", .expected = 4 },
        .{ .input = "const map = {\"foo\":1, \"bar\": 2} const x = map.len", .expected = 2 },
        .{ .input = "mut map = {\"foo\":1} map.add(\"bar\", 2) const x = map.len", .expected = 2 },
    };
    inline for (test_cases) |case| {
        var arena = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const tree = try parser.parse(&arena.allocator, case.input);
        defer tree.deinit();

        var scope = Scope.init(&arena.allocator);
        defer scope.deinit();

        const value = try evalNodes(tree.nodes, &scope);
        testing.expectEqual(@as(i64, case.expected), value.integer);
    }
}

test "Array literal" {
    const input = "[1, 2 * 2, 3 + 3]";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const tree = try parser.parse(&arena.allocator, input);
    defer tree.deinit();

    var scope = Scope.init(&arena.allocator);
    defer scope.deinit();

    const value = try evalNodes(tree.nodes, &scope);
    testing.expect(value == .list);
    testing.expect(value.list.items.len == 3);
    testing.expect(value.list.items[0].integer == 1);
    testing.expect(value.list.items[1].integer == 4);
    testing.expect(value.list.items[2].integer == 6);
}

test "Array index" {
    const input = "[1, 2, 3][0]";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const tree = try parser.parse(&arena.allocator, input);
    defer tree.deinit();

    var scope = Scope.init(&arena.allocator);
    defer scope.deinit();

    const value = try evalNodes(tree.nodes, &scope);
    testing.expect(value.integer == 1);
}

test "Map literal" {
    const input = "const map = {\"foo\":1, \"bar\": 2}";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const tree = try parser.parse(&arena.allocator, input);
    defer tree.deinit();
    var scope = Scope.init(&arena.allocator);
    defer scope.deinit();

    const value = try evalNodes(tree.nodes, &scope);
    const map = value.map;
    testing.expect(map.items().len == 2);
    testing.expect(map.get(Value{ .string = "foo" }).?.integer == 1);
    testing.expect(map.get(Value{ .string = "bar" }).?.integer == 2);
}

test "Map index" {
    const input = "const map = {\"foo\":1, \"bar\": 2} const x = map[\"foo\"]";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const tree = try parser.parse(&arena.allocator, input);
    defer tree.deinit();
    var scope = Scope.init(&arena.allocator);
    defer scope.deinit();

    const value = try evalNodes(tree.nodes, &scope);
    testing.expect(value.integer == 1);
}
