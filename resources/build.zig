const std = @import("std");
const Step = std.Build.Step;

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{ .name = "coffimaker", .root_source_file = b.path("coffimaker.zig"), .target = b.host });
    b.installArtifact(exe);

    exe.linkSystemLibrary("c");

    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);
}
