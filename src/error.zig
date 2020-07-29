const std = @import("std");

//! This contains the logic for errors
//! Errors can be written to a writer using write()

pub const Error = struct {
    fmt: []const u8,
    kind: ErrorType,
    index: usize,

    const ErrorType = enum {
        err,
        note,
        trace,
    };
};

pub const Errors = struct {
    list: std.ArrayList(Error),

    /// Creates a new Errors object that can be catch errors and print them out to a writer
    pub fn init(allocator: *std.mem.Allocator) Errors {
        return .{ .list = std.ArrayList(Error).init(allocator) };
    }

    /// Appends a new error to the error list
    pub fn add(self: *Errors, fmt: []const u8, index: usize, kind: Error.ErrorType) !void {
        return self.list.append(.{
            .fmt = fmt,
            .kind = kind,
            .index = index,
        });
    }

    /// Frees the memory of the errors
    pub fn deinit(self: Errors) !void {
        return self.list.deinit();
    }

    /// Writes the errors to the writer interface
    pub fn write(self: *Errors, writer: anytype) !void {
        //TODO implement
    }
};
