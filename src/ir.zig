const std = @import("std");
const Allocator = std.mem.Allocator;

pub const _value = @import("value.zig");
pub const Type = _value.Type;
pub const Value = _value.Value;

///! This file contains Luf's typed intermediate representation
///! This pass is used by codegen to construct other output such as WASM,
///! Luf bytecode, ASM or other possible future outputs.
pub const Inst = struct {
    /// The type of instruction
    tag: Tag,
    /// The position of the instruction within the source code
    pos: usize,

    /// The kind of instruction
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
        @"for",
        @"while",
        @"switch",
        condition,
        @"break",
        @"continue",
        range,
        list,
        map,
        decl,
        ident,
        primitive,
        @"return",
        negate,
        import,

        /// Returns the type of that belongs to a `Tag`
        /// Can be used to cast to the correct Type from a `Tag`.
        pub fn Type(self: Tag) type {
            return switch (self.tag) {
                .add,
                .sub,
                .mul,
                .div,
                .eql,
                .nql,
                .eql_lt,
                .eql_gt,
                .lt,
                .gt,
                => BinOp,
                .negate, .@"return" => UnOp,
                .primitive => Primitive,
                .func => Function,
                .call => Call,
                .@"for" => Loop,
                .@"while" => While,
                .@"switch" => Switch,
                .condition => If,
                .@"break" => Break,
                .@"continue" => Continue,
                .range => Range,
                .list => List,
                .map => Map,
                .decl => Decl,
                .ident => Identifier,
                .import => Import,
            };
        }
    };

    /// Casts `Inst` into the type that corresponds to `tag`
    /// returns null if tag does not match.
    pub fn castTag(self: *Inst, comptime tag: Tag) ?*tag.Type() {
        if (self.tag != tag) return null;

        return @fieldParentPtr(tag.Type(), "base", self);
    }

    /// Casts `Inst` into `T`
    /// Caller must ensure tags are matching, if unsure
    /// use castTag()
    pub fn as(self: *Inst, comptime T: type) T {
        return @fieldParentPtr(T, "base", self);
    }

    /// Binary operator instruction such as +, -, etc
    pub const BinOp = struct {
        base: Inst,
        lhs: *Inst,
        rhs: *Inst,
    };

    /// Unary operator instruction
    pub const UnOp = struct {
        base: Inst,
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
        decl: *Decl,
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

    pub const If = struct {
        base: Inst,
        condition: *Inst,
        then_block: *Inst,
        else_block: *Inst,
    };

    pub const Primitive = struct {
        base: Inst,
        prim_type: PrimType,

        pub const PrimType = enum {
            @"void",
            @"true",
            @"false",
            nil,
        };
    };

    pub const Assign = struct {
        base: Inst,
        decl: *Decl,
        rhs: *Inst,
    };

    pub const Store = struct {
        base: Inst,
        lhs: *Inst,
        index: *Inst,
        rhs: *Inst,
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

        return &inst.base;
    }

    /// Creates a new `If` instruction and returns its base
    pub fn emitCond(self: *Module, pos: usize, cond: *Inst, then_block: *Inst, else_block: *Inst) *Inst {
        const inst = try self.arena.create(Inst.If);
        inst.* = .{
            .base = .{
                .tag = .condition,
                .pos = pos,
            },
            .condition = cond,
            .then_block = then_block,
            .else_block = else_block,
        };

        return &inst.base;
    }

    /// Creates a new `UnOp` instruction. Expects a prefix `tag` such as '-', or 'return x'
    pub fn emitUnOp(self: *Module, pos: usize, tag: Inst.Tag, rhs: *Inst) *Inst {
        const inst = try self.arena.create(Inst.UnOp);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
            },
            .rhs = rhs,
        };
        return &inst.base;
    }

    /// Creates a new `Block` instruction for the given inner instructions
    pub fn emitBlock(self: *Module, pos: usize, instructions: []*Inst) *Inst {
        const inst = try self.arena.create(Inst.Block);
        inst.* = .{
            .base = .{
                .tag = .block,
                .pos = pos,
            },
            .instructions = instructions,
        };
        return &inst.base;
    }

    /// Creates a new `Primitive` with the given `value`
    pub fn emitPrimitive(self: *Module, pos: usize, value: Inst.Primitive.PrimType) *Inst {
        const inst = try self.arena.create(Inst.Primitive);
        inst.* = .{
            .base = .{
                .tag = .primitive,
                .pos = pos,
            },
            .prim_type = value,
        };
        return &inst.base;
    }

    /// Constructs a new `Declaration` instruction
    pub fn emitDecl(
        self: *Module,
        pos: usize,
        name: []const u8,
        index: usize,
        scope: enum { global, local },
        is_pub: bool,
        is_mut: bool,
        value: *Inst,
    ) *Inst {
        const inst = try self.arena.create(Inst.Decl);
        inst.* = .{
            .base = .{
                .tag = .decl,
                .pos = pos,
            },
            .is_mut = is_mut,
            .is_pub = is_pub,
            .index = index,
            .value = value,
        };
        return &inst.base;
    }

    /// Creates a new `Identifier` instruction
    pub fn emitIdent(self: *Module, pos: usize, decl: *Decl) *Inst {
        const inst = try self.arena.create(Inst.Identifier);
        inst.* = .{
            .base = .{
                .tag = .ident,
                .pos = pos,
            },
            .decl = decl,
        };
        return &inst.base;
    }

    /// Creates a new `List` instruction with `Type` `scalar_type`
    pub fn emitList(self: *Module, pos: usize, scalar_type: Type, elements: []*Inst) *Inst {
        const inst = try self.arena.create(Inst.List);
        inst.* = .{
            .base = .{
                .tag = .list,
                .pos = pos,
            },
            .scalar_type = scalar_type,
            .elements = elements,
        };
        return &inst.base;
    }

    /// Creates a new `Map` instruction where each pair has key type `key_type` and its value is of type `val_type`
    pub fn emitMap(self: *Module, pos: usize, key_type: Type, val_type: Type, pairs: []*Inst) *Inst {
        const inst = try self.arena.create(Inst.Map);
        inst.* = .{
            .base = .{
                .tag = .map,
                .pos = pos,
            },
            .key_type = key_type,
            .val_type = val_type,
            .pairs = pairs,
        };
        return &inst.base;
    }

    /// Creates a new `Index` instruction which is used to load a value from the `lhs`, defined by `rhs`
    pub fn emitIndex(self: *Module, pos: usize, lhs: *Inst, rhs: *Inst) *Inst {
        const inst = try self.arena.create(Inst.Index);
        inst.* = .{
            .base = .{
                .tag = .index,
                .pos = pos,
            },
            .lhs = lhs,
            .rhs = rhs,
        };
        return &inst.base;
    }

    /// Creates a new `Function` instruction
    pub fn emitFunc(
        self: *Module,
        pos: usize,
        block: *Inst,
        locals: usize,
        return_type: Type,
        params: []*Inst,
    ) *Inst {
        const inst = try self.arena.create(Inst.Function);
        inst.* = .{
            .base = .{
                .tag = .func,
                .pos = pos,
            },
            .block = block,
            .locals = locals,
            .return_type = return_type,
            .params = params,
        };
        return &inst.base;
    }

    /// Creates a new function call instruction
    pub fn emitCall(self: *Module, pos: usize, func: *Inst) *Inst {
        const inst = try self.arena.create(Inst.Call);
        inst.* = .{
            .base = .{
                .tag = .call,
                .pos = pos,
            },
            .func = func,
        };
        return &inst.base;
    }

    /// Creates a new `While` instruction
    pub fn emitWhile(self: *Module, pos: usize, block: *Inst, cond: *Inst) *Inst {
        const inst = try self.arena.create(Inst.While);
        inst.* = .{
            .base = .{
                .tag = .@"while",
                .pos = pos,
            },
            .condition = cond,
            .block = block,
        };
        return &inst.base;
    }

    /// Creates a loop instruction
    pub fn emitFor(
        self: *Module,
        pos: usize,
        it_type: Type,
        iterator: *Inst,
        capture: *Inst,
        index: ?*Inst,
        block: *Inst,
    ) *Inst {
        const inst = try self.arena.create(Inst.Loop);
        inst.* = .{
            .base = .{
                .tag = .@"for",
                .pos = pos,
            },
            .it_type = it_type,
            .it = iterator,
            .capture = capture,
            .index = index,
            .block = block,
        };
        return &inst.base;
    }

    /// Creates an assignment instruction
    pub fn emitAssign(self: *Module, pos: usize, decl: *Inst.Decl, rhs: *Inst) *Inst {
        const inst = try self.arena.create(Inst.Assign);
        inst.* = .{
            .base = .{
                .tag = .assign,
                .pos = pos,
            },
            .decl = decl,
            .rhs = rhs,
        };
        return &inst.base;
    }

    /// Creates a store instruction. Used to set the value of an element inside a map or list
    pub fn emitStore(self: *Module, pos: usize, lhs: *Inst, index: *Inst, rhs: *Index) *Inst {
        const inst = try self.arena.create(Inst.Store);
        inst.* = .{
            .base = .{
                .tag = .store,
                .pos = pos,
            },
            .lhs = lhs,
            .index = index,
            .rhs = rhs,
        };
        return &inst.base;
    }

    /// Creates a new Import instruction
    pub fn emitImport(self: *Module, pos: usize, name: []const u8) *Inst {
        const inst = try self.arena.create(Inst.Import);
        inst.* = .{
            .base = .{
                .tag = .import,
                .pos = pos,
            },
            .name = try self.arena.dupe(u8, name),
        };
        return &inst.base;
    }

    /// Creates a `NoOp` instruction, used for control flow such as continue and break
    pub fn emitNoOp(self: *Module, pos: usize, tag: Inst.Tag) *Inst {
        const inst = try self.arena.create(Inst.NoOp);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
            },
        };
        return &inst.base;
    }

    /// Creates a `Range` instruction
    pub fn emitRange(self: *Module, pos: usize, start: *Inst, end: *Inst) *Inst {
        const inst = try self.arena.create(Inst.Range);
        int.* = .{
            .base = .{
                .tag = .range,
                .pos = pos,
            },
            .start = start,
            .end = end,
        };
        return &inst.base;
    }

    /// Constructs a new `Enum` instruction
    pub fn emitEnum(self: *Module, pos: usize, nodes: []*Inst) *Inst {
        const inst = try self.arena.create(Inst.Enum);
        inst.* = .{
            .base = .{
                .tag = .@"enum",
                .pos = pos,
            },
            .value = nodes,
        };
        return &inst.base;
    }
};
