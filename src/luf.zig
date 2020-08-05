pub const Lexer = @import("lexer.zig").Lexer;
pub const Token = @import("token.zig").Token;
pub const repl = @import("repl.zig");

test "All tests" {
    //_ = @import("eval.zig");
    _ = @import("vm.zig");
}
