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

    pub const Tag = enum {
        add,
        assign,
        int,
        string,
        sub,
        mul,
        div,
        eql,
        nql,
        eql_lt,
        eql_gt,
        lt,
        gt,
        assign_add,
        assign_sub,
        assign_mul,
        assign_div,
        func,
        call,

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
        is_pub: bool = false,
        is_mut: bool = false,
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
        value: u64,
    };

    pub const String = struct {
        base: Inst,
        value: []const u8,
    };

    pub const List = struct {
        base: Inst,
        scalar_type: Type,
        elements: []*Inst,
    };

    pub const Map = struct {
        base: Inst,
        key_type: Type,
        val_type: Type,
        pairs: []*Inst,
    };

    pub const Block = struct {
        base: Inst,
        instructions: []*Inst,
    };

    pub const Identifier = struct {
        base: Inst,
        decl: *Inst,
    };

    pub const Pair = struct {
        base: Inst,
        key: *Inst,
        value: *Inst,
    };

    pub const Index = struct {
        base: Inst,
        lhs: *Inst,
        rhs: *Inst,
    };

    pub const Function = struct {
        base: Inst,
        params: []*Inst,
        block: *Inst,
        locals: usize,
        return_type: Type,
    };

    pub const Call = struct {
        base: Inst,
        args: []*Inst,
        func: *Inst,
    };

    pub const Arg = struct {
        base: Inst,
        arg_type: Type,
    };

    pub const Return = struct {
        base: Inst,
        ret_type: Type = ._void,
        value: ?*Inst,
    };

    pub const While = struct {
        base: Inst,
        condition: *Inst,
        block: *Inst,
    };

    pub const Loop = struct {
        base: Inst,
        it_type: Type,
        it: *Inst,
        capture: *Inst,
        index: ?*Inst,
        block: *Inst,
    };

    pub const NoOp = struct {
        base: Inst,
    };

    pub const Import = struct {
        base: Inst,
        name: []const u8,
    };

    pub const Break = struct {
        base: Inst,
        block: *Inst,
    };

    pub const Range = struct {
        base: Inst,
        start: *Inst,
        end: *Inst,
    };

    pub const Continue = struct {
        base: Inst,
        block: *Inst,
    };

    pub const Enum = struct {
        base: Inst,
        value: [][]const u8,
    };

    pub const Switch = struct {
        base: Inst,
        capture: *Inst,
        cap_type: Type,
        prongs: []*Inst,
    };

    pub const Branch = struct {
        base: Inst,
        branch_type: Type,
        value: *Inst,
    };

    pub const Prefix = struct {
        base: Inst,
        rhs: *Inst,
    };

    pub const If = struct {
        base: Inst,
        condition: *Inst,
        then_block: *Inst,
        else_block: *Inst,
    };
};

/// Module contains helper functions to generate IR
/// and contains the final list of instructions.
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

    /// Creates a new `Int` instruction
    pub fn emitInt(self: *Module, pos: usize, value: u64) *Inst {
        const inst = try self.arena.create(Inst.Int);
        int.* = .{
            .base = .{
                .tag = .int,
                .pos = pos,
            },
            .value = value,
        };

        try self.instructions.append(self.arena, &inst.base);

        return &inst.base;
    }

    /// Creates a new `String` instruction. This duplicates the string value
    /// and takes ownership of its memory. Caller must therefore free the original's
    /// string's memory by themselves
    pub fn emitString(self: *Module, pos: usize, value: []const u8) *Inst {
        const inst = try self.arena.create(Inst.String);
        inst.* = .{
            .base = .{
                .tag = .string,
                .pos = pos,
            },
            .value = try self.arena.dupe(u8, value),
        };

        try self.instructions.append(self.arena, &inst.base);
        return &inst.base;
    }
};
