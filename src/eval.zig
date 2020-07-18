const Node = @import("ast.zig").Node;
const Value = @import("value.zig").Value;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const testing = @import("std").testing;

/// Evaluates the given node and returns a Value
pub fn eval(node: Node) Value {
    return switch (node) {
        .expression => |exp| eval(exp.value),
        .int_lit => |lit| Value{ .integer = lit.value },
        .boolean => |bl| Value{ .boolean = bl.value },
        .prefix => |pfx| evalPrefix(pfx),
        else => {
            @import("std").debug.panic("TODO: Implement node: {}\n", .{node});
        },
    };
}

fn evalPrefix(prefix: Node.Prefix) Value {}

test "Eval integer" {
    const test_cases = .{
        .{ .input = "20", .expected = @as(usize, 20) },
        .{ .input = "100", .expected = @as(usize, 100) },
    };

    inline for (test_cases) |case| {
        var lexer = Lexer.init(case.input);
        var parser = try Parser.init(testing.allocator, &lexer);
        const tree = try parser.parse();
        defer tree.deinit();

        const value = eval(tree.nodes[0]);
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

        const value = eval(tree.nodes[0]);
        testing.expectEqual(case.expected, value.boolean);
    }
}
