/// Build in types supported by Luf
pub const Type = enum {
    integer,
    boolean,
    nil,
};

/// Value depending on its type
pub const Value = union(Type) {
    integer: i64,
    boolean: bool,
    nil: void,
};
