pub const Vm = @import("vm.zig").Vm;
pub const byte_code = @import("bytecode.zig");
pub const compiler = @import("compiler.zig");
pub const Errors = @import("error.zig").Errors;

test "All tests" {
    _ = @import("vm.zig");
    _ = @import("wasm.zig");
}
