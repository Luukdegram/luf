const std = @import("std");
const Type = @import("Value.zig").Type;
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
    /// Luf type that corresponts to the instructions
    ty: Type,

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
        func_arg,
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
        slice,

        /// Returns the type of that belongs to a `Tag`
        /// Can be used to cast to the correct Type from a `Tag`.
        pub fn IrType(self: Tag) type {
            return switch (self) {
                .negate,
                .@"return",
                .bitwise_not,
                .not,
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
                .store, .slice => Triple,
                .condition => Condition,
                .primitive => Primitive,
                .func => Function,
                .func_arg => FuncArg,
                .call => Call,
                .@"for" => Loop,
                .@"switch" => Switch,
                .@"break", .@"continue" => NoOp,
                .list, .map => DataStructure,
                .decl => Decl,
                .int => Int,
                .string, .import, .comment => String,
                .block => Block,
                .@"enum" => Enum,
                .ident => Ident,
                .type_def => TypeDef,
            };
        }
    };

    /// Casts `Inst` into the type that corresponds to `tag`
    /// returns null if tag does not match.
    pub fn castTag(self: *Inst, comptime tag: Tag) ?*tag.IrType() {
        if (self.tag != tag) return null;

        return @fieldParentPtr(tag.IrType(), "base", self);
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
        args: []*Inst,
        ret_type: *Inst,
        locals: []*Inst,
    };

    pub const FuncArg = struct {
        base: Inst,
        name: []const u8,
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
        capture: *Inst,
        index: ?*Inst,
    };

    pub const NoOp = struct {
        base: Inst,
    };

    pub const Enum = struct {
        base: Inst,
        value: []*Inst,
    };

    pub const Ident = struct {
        base: Inst,
        index: u32,
        scope: Scope,
        name: []const u8,

        pub const Scope = enum { global, local };
    };

    pub const TypeDef = struct {
        base: Inst,
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
                .ty = .integer,
            },
            .value = value,
        };

        return &inst.base;
    }

    /// Creates a new `String` instruction. This duplicates the string value
    /// and takes ownership of its memory. Caller must therefore free the original's
    /// string's memory by themselves
    pub fn emitString(self: *Module, comptime tag: Inst.Tag, pos: usize, value: []const u8) Error!*Inst {
        const inst = try self.gpa.create(Inst.String);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
                .ty = switch (tag) {
                    .string, .comment => .string,
                    .import => .module,
                    else => @compileError("Unsupported type"),
                },
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
                .ty = ._void,
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
                .ty = switch (value) {
                    .@"true", .@"false" => .boolean,
                    .@"void" => ._void,
                    .nil => .nil,
                },
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
                .ty = value.ty,
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
                .ty = .list,
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
        locals: []*Inst,
        args: []*Inst,
        ret_type: *Inst,
    ) Error!*Inst {
        const inst = try self.gpa.create(Inst.Function);
        inst.* = .{
            .base = .{
                .tag = .func,
                .pos = pos,
                .ty = .function,
            },
            .body = body,
            .locals = locals,
            .args = args,
            .ret_type = ret_type,
        };
        return &inst.base;
    }

    /// Creates a `FuncArg` instruction
    pub fn emitFuncArg(self: *Module, pos: usize, ty: Type, name: []const u8) Error!*Inst {
        const inst = try self.gpa.create(Inst.FuncArg);
        inst.* = .{
            .base = .{
                .tag = .func_arg,
                .pos = pos,
                .ty = ty,
            },
            .name = try self.gpa.dupe(u8, name),
        };
        return &inst.base;
    }

    /// Creates a loop instruction
    pub fn emitFor(
        self: *Module,
        pos: usize,
        iterator: *Inst,
        block: *Inst,
        capture: *Inst,
        index: ?*Inst,
    ) Error!*Inst {
        const inst = try self.gpa.create(Inst.Loop);
        inst.* = .{
            .base = .{
                .tag = .@"for",
                .pos = pos,
                .ty = ._void,
            },
            .it = iterator,
            .block = block,
            .capture = capture,
            .index = index,
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
                .ty = ._void,
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
                .ty = ._enum,
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
                .ty = ._void,
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
                .ty = rhs.ty,
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
                .ty = lhs.ty,
            },
            .lhs = lhs,
            .rhs = rhs,
        };
        return &inst.base;
    }

    /// Emits a `Triple` instruction, that contains the `Tag`, lhs, index and a rhs `Inst`
    /// Used for setting the value of an element inside a list or map
    pub fn emitTriple(
        self: *Module,
        pos: usize,
        tag: Inst.Tag,
        lhs: *Inst,
        index: *Inst,
        rhs: *Inst,
    ) Error!*Inst {
        const inst = try self.gpa.create(Inst.Triple);
        inst.* = .{
            .base = .{
                .tag = tag,
                .pos = pos,
                .ty = rhs.ty,
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
                .ty = ._void,
            },
            .cond = cond,
            .then_block = then_block,
            .else_block = else_block,
        };

        return &inst.base;
    }

    /// Emits a `Call` instruction which calls a function
    pub fn emitCall(self: *Module, ty: Type, pos: usize, func: *Inst, args: []*Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Call);
        inst.* = .{
            .base = .{
                .tag = .call,
                .pos = pos,
                .ty = ty,
            },
            .args = try self.gpa.dupe(*Inst, args),
            .func = func,
        };
        return &inst.base;
    }

    /// Emits an identifier that contains the scope that it was defined in and its index
    pub fn emitIdent(
        self: *Module,
        ty: Type,
        pos: usize,
        name: []const u8,
        scope: Inst.Ident.Scope,
        index: u32,
    ) Error!*Inst {
        const inst = try self.gpa.create(Inst.Ident);
        inst.* = .{
            .base = .{
                .tag = .ident,
                .pos = pos,
                .ty = ty,
            },
            .name = try self.gpa.dupe(u8, name),
            .index = index,
            .scope = scope,
        };

        return &inst.base;
    }

    /// Emits a type definition
    pub fn emitType(self: *Module, ty: Type, pos: usize) Error!*Inst {
        const inst = try self.gpa.create(Inst.TypeDef);
        inst.* = .{
            .base = .{
                .tag = .type_def,
                .pos = pos,
                .ty = ty,
            },
        };
        return &inst.base;
    }

    /// Emits a slice instruction
    pub fn emitSlice(self: *Module, pos: usize, left: *Inst, start: *Inst, end: *Inst) Error!*Inst {
        const inst = try self.gpa.create(Inst.Triple);
        inst.* = .{
            .base = .{
                .tag = .slice,
                .pos = pos,
                .ty = .list,
            },
            .lhs = left,
            .index = start,
            .rhs = end,
        };
        return &inst.base;
    }
};
