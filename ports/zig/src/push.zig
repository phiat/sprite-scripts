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
        \\Usage: sprite-tool push <local-path> <remote-path> [sprite-name]
        \\
        \\Examples:
        \\  sprite-tool push ./file.txt /home/sprite/file.txt
        \\  sprite-tool push ./mydir /home/sprite/mydir
        \\  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk
        \\
    );
}

pub fn execute(alloc: std.mem.Allocator, raw_args: []const []const u8) !void {
    if (raw_args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    const local_path = raw_args[0];
    const remote_path = raw_args[1];
    const sprite_name: ?[]const u8 = if (raw_args.len >= 3) raw_args[2] else null;

    // Check if local path exists and determine if it's a directory
    const is_dir = isDirectory(local_path);
    const exists = is_dir or isFile(local_path);

    if (!exists) {
        const msg = try std.fmt.allocPrint(alloc, "Error: {s} does not exist\n", .{local_path});
        defer alloc.free(msg);
        printErr(msg);
        std.process.exit(1);
    }

    if (is_dir) {
        const msg = try std.fmt.allocPrint(alloc, "Pushing directory: {s} -> {s}\n", .{ local_path, remote_path });
        defer alloc.free(msg);
        printMsg(msg);
        try pushDirectory(alloc, sprite_name, local_path, remote_path);
    } else {
        const msg = try std.fmt.allocPrint(alloc, "Pushing file: {s} -> {s}\n", .{ local_path, remote_path });
        defer alloc.free(msg);
        printMsg(msg);
        try pushSingleFile(alloc, sprite_name, local_path, remote_path);
    }

    printMsg("Done.\n");
}

fn isDirectory(path: []const u8) bool {
    var dir = std.fs.cwd().openDir(path, .{}) catch return false;
    dir.close();
    return true;
}

fn isFile(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

fn pushDirectory(alloc: std.mem.Allocator, sprite_name: ?[]const u8, local_path: []const u8, remote_path: []const u8) !void {
    const dir_name = std.fs.path.dirname(local_path) orelse ".";
    const base_name = std.fs.path.basename(local_path);

    // tar on local side
    const tar_argv = &[_][]const u8{ "tar", "czf", "-", "-C", dir_name, base_name };
    var tar_child = std.process.Child.init(tar_argv, alloc);
    tar_child.stderr_behavior = .Inherit;
    tar_child.stdout_behavior = .Pipe;
    tar_child.stdin_behavior = .Inherit;
    try tar_child.spawn();

    // sprite exec to untar on remote
    const untar_cmd = try std.fmt.allocPrint(alloc, "mkdir -p '{s}' && tar xzf - -C '{s}' --strip-components=1", .{ remote_path, remote_path });
    defer alloc.free(untar_cmd);

    var sprite_argv_list: std.ArrayList([]const u8) = .empty;
    defer sprite_argv_list.deinit(alloc);
    try sprite_argv_list.append(alloc,"sprite");
    try sprite_argv_list.append(alloc,"exec");
    if (sprite_name) |name| {
        try sprite_argv_list.append(alloc,"-s");
        try sprite_argv_list.append(alloc,name);
    }
    try sprite_argv_list.append(alloc,"bash");
    try sprite_argv_list.append(alloc,"-c");
    try sprite_argv_list.append(alloc,untar_cmd);

    var sprite_child = std.process.Child.init(sprite_argv_list.items, alloc);
    sprite_child.stderr_behavior = .Inherit;
    sprite_child.stdout_behavior = .Inherit;
    sprite_child.stdin_behavior = .Pipe;
    try sprite_child.spawn();

    // Pipe tar stdout to sprite stdin
    const tar_stdout = tar_child.stdout.?;
    const sprite_stdin = sprite_child.stdin.?;
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = tar_stdout.read(&buf) catch break;
        if (n == 0) break;
        _ = sprite_stdin.write(buf[0..n]) catch break;
    }
    sprite_stdin.close();
    sprite_child.stdin = null;

    _ = try tar_child.wait();
    const term = try sprite_child.wait();
    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                printErr("Error: push directory failed\n");
                std.process.exit(1);
            }
        },
        else => {
            printErr("Error: push directory failed\n");
            std.process.exit(1);
        },
    }
}

fn pushSingleFile(alloc: std.mem.Allocator, sprite_name: ?[]const u8, local_path: []const u8, remote_path: []const u8) !void {
    const remote_dir = std.fs.path.dirname(remote_path) orelse "/";
    const cat_cmd = try std.fmt.allocPrint(alloc, "mkdir -p '{s}' && cat > '{s}'", .{ remote_dir, remote_path });
    defer alloc.free(cat_cmd);

    var argv_list: std.ArrayList([]const u8) = .empty;
    defer argv_list.deinit(alloc);
    try argv_list.append(alloc,"sprite");
    try argv_list.append(alloc,"exec");
    if (sprite_name) |name| {
        try argv_list.append(alloc,"-s");
        try argv_list.append(alloc,name);
    }
    try argv_list.append(alloc,"bash");
    try argv_list.append(alloc,"-c");
    try argv_list.append(alloc,cat_cmd);

    var child = std.process.Child.init(argv_list.items, alloc);
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stdin_behavior = .Pipe;
    try child.spawn();

    // Open local file and pipe to child stdin
    const local_file = try std.fs.cwd().openFile(local_path, .{});
    defer local_file.close();

    const child_stdin = child.stdin.?;
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = local_file.read(&buf) catch break;
        if (n == 0) break;
        _ = child_stdin.write(buf[0..n]) catch break;
    }
    child_stdin.close();
    child.stdin = null;

    const term = try child.wait();
    switch (term) {
        .Exited => |code| {
            if (code != 0) {
                printErr("Error: push file failed\n");
                std.process.exit(1);
            }
        },
        else => {
            printErr("Error: push file failed\n");
            std.process.exit(1);
        },
    }
}
