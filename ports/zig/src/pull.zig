const std = @import("std");
const sprite = @import("sprite.zig");

fn printMsg(msg: []const u8) void {
    std.fs.File.stdout().writeAll(msg) catch {};
}

fn printErr(msg: []const u8) void {
    std.fs.File.stderr().writeAll(msg) catch {};
}

fn printUsage() void {
    printErr(
        \\Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]
        \\
        \\Examples:
        \\  sprite-tool pull /home/sprite/file.txt ./file.txt
        \\  sprite-tool pull /home/sprite/mydir ./mydir
        \\  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk
        \\
    );
}

pub fn execute(alloc: std.mem.Allocator, raw_args: []const []const u8) !void {
    if (raw_args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    const remote_path = raw_args[0];
    const local_path = raw_args[1];
    const sprite_name: ?[]const u8 = if (raw_args.len >= 3) raw_args[2] else null;

    // Check if remote path is a directory
    const check_cmd = try std.fmt.allocPrint(alloc, "[ -d '{s}' ] && echo 'dir' || echo 'file'", .{remote_path});
    defer alloc.free(check_cmd);

    const is_dir_output = try sprite.sxCapture(alloc, sprite_name, check_cmd);
    const is_dir = if (is_dir_output) |output| blk: {
        defer alloc.free(output);
        break :blk std.mem.eql(u8, output, "dir");
    } else false;

    if (is_dir) {
        const msg = try std.fmt.allocPrint(alloc, "Pulling directory: {s} -> {s}\n", .{ remote_path, local_path });
        defer alloc.free(msg);
        printMsg(msg);
        try pullDirectory(alloc, sprite_name, remote_path, local_path);
    } else {
        const msg = try std.fmt.allocPrint(alloc, "Pulling file: {s} -> {s}\n", .{ remote_path, local_path });
        defer alloc.free(msg);
        printMsg(msg);
        try pullFile(alloc, sprite_name, remote_path, local_path);
    }

    printMsg("Done.\n");
}

fn pullDirectory(alloc: std.mem.Allocator, sprite_name: ?[]const u8, remote_path: []const u8, local_path: []const u8) !void {
    // mkdir -p local_path
    std.fs.cwd().makePath(local_path) catch |err| {
        const msg = std.fmt.allocPrint(alloc, "Error: cannot create local directory {s}: {}\n", .{ local_path, err }) catch {
            printErr("Error: cannot create local directory\n");
            std.process.exit(1);
        };
        defer alloc.free(msg);
        printErr(msg);
        std.process.exit(1);
    };

    // sprite exec tar on remote
    const tar_cmd = try std.fmt.allocPrint(alloc, "tar czf - -C '{s}' .", .{remote_path});
    defer alloc.free(tar_cmd);

    var sprite_argv_list = std.ArrayList([]const u8).init(alloc);
    defer sprite_argv_list.deinit();
    try sprite_argv_list.append("sprite");
    try sprite_argv_list.append("exec");
    if (sprite_name) |name| {
        try sprite_argv_list.append("-s");
        try sprite_argv_list.append(name);
    }
    try sprite_argv_list.append("bash");
    try sprite_argv_list.append("-c");
    try sprite_argv_list.append(tar_cmd);

    var sprite_child = std.process.Child.init(sprite_argv_list.items, alloc);
    sprite_child.stderr_behavior = .Inherit;
    sprite_child.stdout_behavior = .Pipe;
    sprite_child.stdin_behavior = .Inherit;
    try sprite_child.spawn();

    // Local tar to extract
    const tar_extract_argv = &[_][]const u8{ "tar", "xzf", "-", "-C", local_path };
    var tar_child = std.process.Child.init(tar_extract_argv, alloc);
    tar_child.stderr_behavior = .Inherit;
    tar_child.stdout_behavior = .Inherit;
    tar_child.stdin_behavior = .Pipe;
    try tar_child.spawn();

    // Pipe sprite stdout to tar stdin
    const sprite_stdout = sprite_child.stdout.?;
    const tar_stdin = tar_child.stdin.?;
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = sprite_stdout.read(&buf) catch break;
        if (n == 0) break;
        _ = tar_stdin.write(buf[0..n]) catch break;
    }
    tar_stdin.close();
    tar_child.stdin = null;

    _ = try sprite_child.wait();
    const term = try tar_child.wait();
    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                printErr("Error: pull directory failed\n");
                std.process.exit(1);
            }
        },
        else => {
            printErr("Error: pull directory failed\n");
            std.process.exit(1);
        },
    }
}

fn pullFile(alloc: std.mem.Allocator, sprite_name: ?[]const u8, remote_path: []const u8, local_path: []const u8) !void {
    // mkdir -p dirname(local_path)
    const local_dir = std.fs.path.dirname(local_path) orelse ".";
    std.fs.cwd().makePath(local_dir) catch |err| {
        const msg = std.fmt.allocPrint(alloc, "Error: cannot create directory {s}: {}\n", .{ local_dir, err }) catch {
            printErr("Error: cannot create directory\n");
            std.process.exit(1);
        };
        defer alloc.free(msg);
        printErr(msg);
        std.process.exit(1);
    };

    // sprite exec cat remote_path > local_path
    var argv_list = std.ArrayList([]const u8).init(alloc);
    defer argv_list.deinit();
    try argv_list.append("sprite");
    try argv_list.append("exec");
    if (sprite_name) |name| {
        try argv_list.append("-s");
        try argv_list.append(name);
    }
    try argv_list.append("cat");
    try argv_list.append(remote_path);

    var child = std.process.Child.init(argv_list.items, alloc);
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Pipe;
    child.stdin_behavior = .Inherit;
    try child.spawn();

    // Write child stdout to local file
    const local_file = try std.fs.cwd().createFile(local_path, .{});
    defer local_file.close();

    const child_stdout = child.stdout.?;
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = child_stdout.read(&buf) catch break;
        if (n == 0) break;
        _ = local_file.write(buf[0..n]) catch break;
    }

    const term = try child.wait();
    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                printErr("Error: pull file failed\n");
                std.process.exit(1);
            }
        },
        else => {
            printErr("Error: pull file failed\n");
            std.process.exit(1);
        },
    }
}
