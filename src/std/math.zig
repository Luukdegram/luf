const math = @import("std").math;

pub fn log(base: i64, x: i64) i64 {
    return math.log(i64, base, x);
}
