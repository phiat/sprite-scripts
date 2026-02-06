const std = @import("std");
const launch = @import("launch.zig");
const push = @import("push.zig");
const pull = @import("pull.zig");
const watch = @import("watch.zig");

fn printUsage() void {
    std.fs.File.stderr().writeAll(
        \\Usage: sprite-tool <command> [args...]
        \\
        \\Commands:
        \\  launch    Create and configure a sprite with coding agent, git, beads
        \\  push      Push local file or directory to a sprite
        \\  pull      Pull file or directory from a sprite
        \\  watch     Poll a sprite's beads tracker task for progress
        \\
        \\Run 'sprite-tool <command> --help' for more information on a command.
        \\
    ) catch {};
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // Collect all args into a slice (skip argv[0] which is the program name)
    var arg_list = std.ArrayList([]const u8).init(alloc);
    defer arg_list.deinit();

    var args_iter = std.process.args();
    _ = args_iter.skip(); // skip program name

    while (args_iter.next()) |arg| {
        try arg_list.append(arg);
    }

    const args = arg_list.items;

    if (args.len == 0) {
        printUsage();
        std.process.exit(1);
    }

    const command = args[0];
    const sub_args = args[1..];

    if (std.mem.eql(u8, command, "launch")) {
        try launch.execute(alloc, sub_args);
    } else if (std.mem.eql(u8, command, "push")) {
        try push.execute(alloc, sub_args);
    } else if (std.mem.eql(u8, command, "pull")) {
        try pull.execute(alloc, sub_args);
    } else if (std.mem.eql(u8, command, "watch")) {
        try watch.execute(alloc, sub_args);
    } else if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        printUsage();
    } else {
        const msg = std.fmt.allocPrint(alloc, "Unknown command: {s}\n\n", .{command}) catch {
            std.fs.File.stderr().writeAll("Unknown command\n") catch {};
            printUsage();
            std.process.exit(1);
        };
        defer alloc.free(msg);
        std.fs.File.stderr().writeAll(msg) catch {};
        printUsage();
        std.process.exit(1);
    }
}
