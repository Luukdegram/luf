const std = @import("std");
const Allocator = std.mem.Allocator;

//! This file contains Luf's typed intermediate representation
//! This pass is used by codegen to construct other output such as WASM,
//! Luf bytecode, ASM or other possible future outputs.

/// General instruction struct. Contains the type (tag) of the instruction,
/// and also the source code position, which can be used for debug symbols.
/// `Inst` is used as a child pointer to the actual Instruction set.
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
        @"enum",
        range,
        list,
        map,
        decl,
        ident,
        primitive,
        @"return",
        negate,
        import,
        mod,
        @"and",
        @"or",
        bitwise_xor,
        bitwise_or,
        bitwise_and,
        bitwise_not,
        shift_left,
        shift_right,
        not,
        block,
        comment,
        store,
        pair,
        load,
        type_def,
        branch,
        expr,

        /// Returns the type of that belongs to a `Tag`
        /// Can be used to cast to the correct Type from a `Tag`.
        pub fn Type(self: Tag) type {
            return switch (self) {
                .negate,
                .@"return",
                .bitwise_not,
                .not,
                .ident,
                .expr,
                => Single,
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
                .@"while",
                .range,
                .mod,
                .@"and",
                .@"or",
                .bitwise_xor,
                .bitwise_or,
                .bitwise_and,
                .shift_left,
                .shift_right,
                .assign_add,
                .assign_sub,
                .assign_mul,
                .assign_div,
                .pair,
                .load,
                .branch,
                .assign,
                => Double,
                .store => Triple,
                .condition => Condition,
                .primitive => Primitive,
                .func => Function,
                .call => Call,
                .@"for" => Loop,
                .@"switch" => Switch,
                .@"break", .@"continue", .type_def => NoOp,
                .list, .map => DataStructure,
                .decl => Decl,
                .int => Int,
                .string, .import, .comment => String,
                .block => Block,
                .@"enum" => Enum,
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
    pub fn as(self: *Inst, comptime T: type) *T {
        return @fieldParentPtr(T, "base", self);
    }

    /// Declaration which contains the name, position, scope and value
    pub const Decl = struct {
        base: Inst,
        name: []const u8,
        is_pub: bool = false,
        is_mut: bool = false,
        scope: Scope,
        /// position within the global or local scope
        index: u32,
        value: *Inst,

        pub const Scope = enum { global, local };
    };

    pub const Int = struct {
        base: Inst,
        value: u64,
    };

    pub const String = struct {
        base: Inst,
        value: []const u8,
    };

    pub const DataStructure = struct {
        base: Inst,
        elements: []*Inst,
    };

    pub const Block = struct {
        base: Inst,
        instructions: []*Inst,
    };

    pub const Function = struct {
        base: Inst,
        body: *Inst,
        locals: usize,
        params: usize,
    };

    pub const Call = struct {
        base: Inst,
        args: []*Inst,
        func: *Inst,
    };

    pub const Loop = struct {
        base: Inst,
        it: *Inst,
        block: *Inst,
        has_index: bool,
    };

    pub const NoOp = struct {
        base: Inst,
    };

    pub const Enum = struct {
        base: Inst,
        value: []*Inst,
    };

    pub const Switch = struct {
        base: Inst,
        capture: *Inst,
        branches: []*Inst,
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

    pub const Single = struct {
        base: Inst,
        rhs: *Inst,
    };

    pub const Double = struct {
        base: Inst,
        lhs: *Inst,
        rhs: *Inst,
    };

    pub const Triple = struct {
        base: Inst,
        lhs: *Inst,
        index: *Inst,
        rhs: *Inst,
    };

    pub const Condition = struct {
        base: Inst,
        cond: *Inst,
        then_block: *Inst,
        else_block: ?*Inst,
    };
};

pub const Instructions = std.ArrayListUnmanaged(*Inst);

/// Final compilation unit. Contains a list of all IR instructions
pub const CompileUnit = struct {
    /// the final instructions list
    instructions: []const *Inst,
    /// The allocator used to create the instructions
    gpa: *Allocator,
    /// state used to free all instructions at once
    state: std.heap.ArenaAllocator.State,

    /// Frees memory of generated instructions
    pub fn deinit(self: *CompileUnit) void {
        self.state.promote(self.gpa).deinit();
        self.gpa.free(self.instructions);
        self.* = undefined;
    }
};

/// Module contains helper functions to generate IR
pub const Module = struct {
    /// Allocator to create new instructions
    gpa: *Allocator,

    pub const Error = error{OutOfMemory};

    /// Creates a new `Int` instruction
    pub fn emitInt(self: *Module, pos: usize, value: u64) Error!*Inst {
        const inst = try self.gpa.create(Inst.Int);
        inst.* = .{
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
    pub fn emitString(self: *Module, tag: Inst.Tag, pos: usize, value: []const u8) Error!*Inst {
        const inst = try self.gpa.create(Inst.String);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
            },
            .value = try self.gpa.dupe(u8, value),
        };

        return &inst.base;
    }

    /// Creates a new `Block` instruction for the given inner instructions
    pub fn emitBlock(self: *Module, pos: usize, instructions: []*Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Block);
        inst.* = .{
            .base = .{
                .tag = .block,
                .pos = pos,
            },
            .instructions = try self.gpa.dupe(*Inst, instructions),
        };
        return &inst.base;
    }

    /// Creates a new `Primitive` with the given `value`
    pub fn emitPrimitive(self: *Module, pos: usize, value: Inst.Primitive.PrimType) Error!*Inst {
        const inst = try self.gpa.create(Inst.Primitive);
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
        index: u32,
        scope: Inst.Decl.Scope,
        is_pub: bool,
        is_mut: bool,
        value: *Inst,
    ) Error!*Inst {
        const inst = try self.gpa.create(Inst.Decl);
        inst.* = .{
            .base = .{
                .tag = .decl,
                .pos = pos,
            },
            .scope = scope,
            .name = try self.gpa.dupe(u8, name),
            .is_mut = is_mut,
            .is_pub = is_pub,
            .index = index,
            .value = value,
        };
        return &inst.base;
    }

    /// Creates a new `List` instruction with `Type` `scalar_type`
    pub fn emitList(self: *Module, tag: Inst.Tag, pos: usize, elements: []*Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.DataStructure);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
            },
            .elements = try self.gpa.dupe(*Inst, elements),
        };
        return &inst.base;
    }

    /// Creates a new `Function` instruction
    pub fn emitFunc(
        self: *Module,
        pos: usize,
        body: *Inst,
        locals: usize,
        params: usize,
    ) Error!*Inst {
        const inst = try self.gpa.create(Inst.Function);
        inst.* = .{
            .base = .{
                .tag = .func,
                .pos = pos,
            },
            .body = body,
            .locals = locals,
            .params = params,
        };
        return &inst.base;
    }

    /// Creates a loop instruction
    pub fn emitFor(
        self: *Module,
        pos: usize,
        iterator: *Inst,
        block: *Inst,
        has_index: bool,
    ) Error!*Inst {
        const inst = try self.gpa.create(Inst.Loop);
        inst.* = .{
            .base = .{
                .tag = .@"for",
                .pos = pos,
            },
            .it = iterator,
            .block = block,
            .has_index = has_index,
        };
        return &inst.base;
    }

    /// Creates an assignment instruction
    pub fn emitAssign(self: *Module, pos: usize, decl: *Inst, rhs: *Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Assign);
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

    /// Creates a `NoOp` instruction, used for control flow such as continue and break
    pub fn emitNoOp(self: *Module, pos: usize, tag: Inst.Tag) Error!*Inst {
        const inst = try self.gpa.create(Inst.NoOp);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
            },
        };
        return &inst.base;
    }

    /// Constructs a new `Enum` instruction
    pub fn emitEnum(self: *Module, pos: usize, nodes: []*Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Enum);
        inst.* = .{
            .base = .{
                .tag = .@"enum",
                .pos = pos,
            },
            .value = try self.gpa.dupe(*Inst, nodes),
        };
        return &inst.base;
    }

    /// Creates a `Switch` instruction
    pub fn emitSwitch(self: *Module, pos: usize, capture: *Inst, branches: []*Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Switch);
        inst.* = .{
            .base = .{
                .tag = .@"switch",
                .pos = pos,
            },
            .capture = capture,
            .branches = try self.gpa.dupe(*Inst, branches),
        };
        return &inst.base;
    }

    /// Emits a `Single` instruction, that contains the tag and a rhs `Inst`
    /// Used for unary operations such as a negate or 'return x'
    pub fn emitSingle(self: *Module, pos: usize, tag: Inst.Tag, rhs: *Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Single);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
            },
            .rhs = rhs,
        };
        return &inst.base;
    }

    /// Emits a `Double` instruction, that contains the `Tag`, lhs and rhs `Inst`
    /// Used to set a lhs value, or retrieve a value from a list or map
    pub fn emitDouble(self: *Module, pos: usize, tag: Inst.Tag, lhs: *Inst, rhs: *Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Double);
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

    /// Emits a `Triple` instruction, that contains the `Tag`, lhs, index and a rhs `Inst`
    /// Used for setting the value of an element inside a list or map
    pub fn emitTriple(self: *Module, pos: usize, tag: Inst.Tag, lhs: *Inst, index: *Inst, rhs: *Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Triple);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
            },
            .lhs = lhs,
            .index = index,
            .rhs = rhs,
        };
        return &inst.base;
    }

    /// Emits a `Condition` instruction, used for if expressions
    pub fn emitCond(self: *Module, pos: usize, cond: *Inst, then_block: *Inst, else_block: ?*Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Condition);
        inst.* = .{
            .base = .{
                .tag = .condition,
                .pos = pos,
            },
            .cond = cond,
            .then_block = then_block,
            .else_block = else_block,
        };

        return &inst.base;
    }

    /// Emites a `Call` instruction which calls a function
    pub fn emitCall(self: *Module, pos: usize, func: *Inst, args: []*Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Call);
        inst.* = .{
            .base = .{
                .tag = .call,
                .pos = pos,
            },
            .args = try self.gpa.dupe(*Inst, args),
            .func = func,
        };
        return &inst.base;
    }
};
