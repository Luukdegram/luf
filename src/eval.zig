const Node = @import("ast.zig").Node;
const Value = @import("value.zig").Value;
const Type = @import("value.zig").Type;
const Env = @import("value.zig").Env;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;

pub const EvalError = error{
    UnknownOperator,
    TypeMismatch,
};

/// Evaluates the given node and returns a Value
/// TODO Error handling
pub fn eval(node: Node, env: *Env) Value {
    return switch (node) {
        .expression => |exp| eval(exp.value, env),
        .int_lit => |lit| Value{ .integer = @bitCast(i64, lit.value) },
        .boolean => |bl| Value{ .boolean = bl.value },
        .prefix => |pfx| evalPrefix(pfx, env),
        .block_statement => |block| evalBlock(block, env),
        .if_expression => |if_exp| evalIf(if_exp, env),
        .infix => |ifx| evalInfix(ifx, env),
        ._return => |ret| Value{ ._return = &eval(ret.value, env) },
        .identifier => |id| evalIdentifier(id, env),
        .declaration => |decl| {
            const val = eval(decl.value, env);
            // TODO Error handling
            env.set(decl.name.identifier.value, val) catch {};
            return val;
        },
        else => {
            @import("std").debug.panic("TODO: Implement node: {}\n", .{node});
        },
    };
}

/// Evaluates the nodes and returns the final Value
pub fn evalNodes(nodes: []Node, env: *Env) Value {
    var value: Value = undefined;
    for (nodes) |node| {
        value = eval(node, env);

        if (value == ._return) {
            return value._return.*;
        }
    }

    return value;
}

fn evalBlock(block: *const Node.BlockStatement, env: *Env) Value {
    var value: Value = undefined;
    for (block.nodes) |node| {
        value = eval(node, env);

        if (value == ._return and value._return.* != .nil) {
            return value;
        }
    }

    return value;
}

fn evalPrefix(prefix: *const Node.Prefix, env: *Env) Value {
    return switch (prefix.operator) {
        .bang => evalBangOperator(eval(prefix.right, env)),
        .minus => evalNegation(eval(prefix.right, env)),
        else => Value{ .nil = {} },
    };
}

fn evalInfix(infix: *const Node.Infix, env: *Env) Value {
    const left = eval(infix.left, env);
    const right = eval(infix.right, env);

    return switch (resolveType(left, right)) {
        .integer => evalIntegerInfix(infix.operator, left, right),
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

fn evalIdentifier(identifier: *const Node.Identifier, env: *Env) Value {
    if (env.get(identifier.value)) |val| {
        return val;
    } else {
        //TODO Error handling
        return Value{ .nil = {} };
    }
}

fn evalIf(exp: *const Node.IfExpression, env: *Env) Value {
    const condition = eval(exp.condition, env);

    if (isTrue(condition)) {
        return eval(exp.true_pong, env);
    } else if (exp.false_pong) |pong| {
        return eval(pong, env);
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
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(testing.allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();
        var env = Env.init(testing.allocator);
        defer env.deinit();

        const value = evalNodes(tree.nodes, &env);
        testing.expectEqual(case.expected, value.integer);
    }
}

test "Eval boolean" {
    const test_cases = .{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
    };

    inline for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(testing.allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();
        var env = Env.init(testing.allocator);
        defer env.deinit();

        const value = evalNodes(tree.nodes, &env);
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
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(testing.allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();
        var env = Env.init(testing.allocator);
        defer env.deinit();

        const value = evalNodes(tree.nodes, &env);
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
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(testing.allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();
        var env = Env.init(testing.allocator);
        defer env.deinit();

        const value = evalNodes(tree.nodes, &env);
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
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(testing.allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();
        var env = Env.init(testing.allocator);
        defer env.deinit();

        const value = evalNodes(tree.nodes, &env);
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
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(testing.allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();
        var env = Env.init(testing.allocator);
        defer env.deinit();

        const value = evalNodes(tree.nodes, &env);
        testing.expectEqual(@intCast(i64, case.expected), value.integer);
    }
}
