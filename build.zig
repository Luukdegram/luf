const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const lib = b.addStaticLibrary("luf", "src/luf.zig");
    lib.setBuildMode(mode);
    lib.install();

    var main_tests = b.addTest("src/luf.zig");
    main_tests.setBuildMode(mode);
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    var repl = b.addExecutable("luf", "repl/main.zig");
    repl.addPackagePath("luf", "src/luf.zig");
    repl.setBuildMode(mode);
    repl.install();

    const repl_runner = repl.run();
    repl_runner.step.dependOn(b.getInstallStep());
    const repl_step = b.step("repl", "Run Luf's REPL");
    repl_step.dependOn(&repl_runner.step);
}
