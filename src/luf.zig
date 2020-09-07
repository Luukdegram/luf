pub const Lexer = @import("lexer.zig").Lexer;
pub const Token = @import("token.zig").Token;
pub const Vm = @import("vm.zig").Vm;

test "All tests" {
    _ = @import("vm.zig");
}
