const math = @import("std").math;

pub fn log(base: u64, x: u64) u64 {
    return math.log(u64, base, x);
}
