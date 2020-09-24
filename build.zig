const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const lib = b.addStaticLibrary("luf", "src/luf.zig");
    lib.setBuildMode(mode);
    lib.install();

    var examples = b.addExecutable("example", "examples/example_runner.zig");
    examples.setBuildMode(mode);
    examples.addPackagePath("luf", "src/luf.zig");
    const example_runner = examples.run();

    const example = b.option([]const u8, "example", "The example to run (without .luf suffix)");

    examples.addBuildOption(?[]const u8, "example", example);

    const example_step = b.step("examples", "Runs an example using the -Dexample flag");
    example_step.dependOn(&example_runner.step);

    var main_tests = b.addTest("src/luf.zig");
    main_tests.setBuildMode(mode);
    main_tests.setTarget(.{ .os_tag = .windows });
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
