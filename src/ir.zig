pub const _value = @import("value.zig");
pub const Type = _value.Type;
pub const Value = _value.Value;

///! This file contains Luf's intermediate representation
///! This pass is used by codegen to construct other output such as WASM,
///! Luf bytecode, ASM or other possible future outputs.
pub const Inst = struct {
    tag: Tag,

    inst_type: Type,

    pub const Tag = enum {
        add,
        assign,

        pub fn Type(self: Tag) type {}
    };

    pub const BinOp = struct {
        base: Inst,
        lhs: *Inst,
        rhs: *Inst,
    };
};
