/// Build in types supported by Luf
pub const Type = enum {
    integer,
    boolean,
    nil,
    _return,
};

/// Value depending on its type
pub const Value = union(Type) {
    integer: i64,
    boolean: bool,
    nil: void,
    _return: *Value,
};
