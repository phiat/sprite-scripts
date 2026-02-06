const std = @import("std");

pub fn main() !void {
    // Test 1: Child process init and spawn
    const argv = [_][]const u8{ "echo", "hello" };
    var child = std.process.Child.init(&argv, std.heap.page_allocator);
    child.stdin = .pipe;
    child.stdout = .pipe;
    child.stderr = .inherit;
    try child.spawn();
    const term = try child.wait();
    _ = term;

    // Test 2: setenv
    try std.posix.setenv("FOO", "bar");

    // Test 3: reader
    const stdout = std.io.getStdOut().writer();
    try stdout.print("hello\n", .{});
}
