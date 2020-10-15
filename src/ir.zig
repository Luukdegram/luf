const std = @import("std");
const Allocator = std.mem.Allocator;

pub const _value = @import("value.zig");
pub const Type = _value.Type;
pub const Value = _value.Value;

///! This file contains Luf's intermediate representation
///! This pass is used by codegen to construct other output such as WASM,
///! Luf bytecode, ASM or other possible future outputs.
pub const Inst = struct {
    /// The type of instruction
    tag: Tag,
    /// The position of the instruction within the source code
    pos: usize,
    ///
    inst_type: Type,

    pub const Tag = enum {
        add,
        assign,

        pub fn Type(self: Tag) type {}
    };

    /// Casts `Inst` into the type that corresponds to `tag`
    pub fn castTag(self: *Inst, comptime tag: Tag) ?*tag.Type() {
        if (self.tag != tag) return null;

        return @fieldParentPtr(tag.Type(), "base", self);
    }

    /// Binary operator instruction such as +, -, etc
    pub const BinOp = struct {
        base: Inst,
        lhs: *Inst,
        rhs: *Inst,
    };

    /// Declaration which contains the name, position, scope and value
    pub const Decl = struct {
        base: Inst,
        name: []const u8,
        scope: enum {
            global,
            local,
        },
        /// position within the global or local scope
        index: usize,
        value: *Inst,
    };

    pub const Int = struct {
        base: Inst,
        value: i64,
    };

    pub const String = struct {
        base: Inst,
        value: []const u8,
    };

    pub const List = struct {
        base: Inst,
        scalar_type: Type,
    };
};

/// Module contains helper functions to generate IR
/// and contains the final list of instructions.
///
pub const Module = struct {
    instructions: std.ArrayListUnmanaged(*Inst),
    arena: *Allocator,

    /// Adds a binary operator instruction to the module and returns the newly created Instruction
    pub fn emitBinOp(self: *Module, pos: usize, tag: Inst.Tag, lhs: *Inst, rhs: *Inst) *Inst {
        const inst = try self.arena.create(Inst.BinOp);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
            },
            .lhs = lhs,
            .rhs = rhs,
        };

        try self.instructions.append(self.arena, &inst.base);

        return &inst.base;
    }
};
