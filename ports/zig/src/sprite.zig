const std = @import("std");

/// Check if a Child process terminated with exit code 0.
fn exitedOk(term: std.process.Child.Term) bool {
    return switch (term) {
        .Exited => |code| code == 0,
        else => false,
    };
}

/// Run a command, wait for it to finish.
/// Returns true if the process exited with code 0.
pub fn run(alloc: std.mem.Allocator, argv: []const []const u8) !bool {
    var child = std.process.Child.init(argv, alloc);
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stdin_behavior = .Inherit;
    try child.spawn();
    const term = try child.wait();
    return exitedOk(term);
}

/// Run a command and capture its stdout as a string. Caller owns returned slice.
/// Returns null on non-zero exit.
pub fn runCapture(alloc: std.mem.Allocator, argv: []const []const u8) !?[]const u8 {
    var child = std.process.Child.init(argv, alloc);
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Pipe;
    child.stdin_behavior = .Inherit;
    try child.spawn();

    // Read all stdout using the raw File.read in a loop
    const output = try readAllFromFile(alloc, child.stdout.?);

    const term = try child.wait();
    if (!exitedOk(term)) {
        alloc.free(output);
        return null;
    }

    // Trim trailing whitespace
    const trimmed = std.mem.trimRight(u8, output, "\n\r \t");
    if (trimmed.len < output.len) {
        const result = try alloc.dupe(u8, trimmed);
        alloc.free(output);
        return result;
    }
    return output;
}

/// Run a command and capture its stdout, suppressing stderr too.
/// Returns null on failure.
pub fn runCaptureQuiet(alloc: std.mem.Allocator, argv: []const []const u8) !?[]const u8 {
    var child = std.process.Child.init(argv, alloc);
    child.stderr_behavior = .Ignore;
    child.stdout_behavior = .Pipe;
    child.stdin_behavior = .Inherit;
    try child.spawn();

    const output = try readAllFromFile(alloc, child.stdout.?);

    const term = try child.wait();
    if (!exitedOk(term)) {
        alloc.free(output);
        return null;
    }

    const trimmed = std.mem.trimRight(u8, output, "\n\r \t");
    if (trimmed.len < output.len) {
        const result = try alloc.dupe(u8, trimmed);
        alloc.free(output);
        return result;
    }
    return output;
}

/// Read all data from a File into an allocated buffer.
fn readAllFromFile(alloc: std.mem.Allocator, file: std.fs.File) ![]u8 {
    var list = std.ArrayList(u8).init(alloc);
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = file.read(&buf) catch |err| switch (err) {
            error.BrokenPipe => break,
            else => return err,
        };
        if (n == 0) break;
        try list.appendSlice(buf[0..n]);
    }
    return list.toOwnedSlice();
}

/// Execute a bash command on a sprite: sprite exec -s <name> bash -c <cmd>
/// If sprite_name is null, omit the -s flag.
pub fn sx(alloc: std.mem.Allocator, sprite_name: ?[]const u8, cmd: []const u8, dry_run: bool) !bool {
    if (dry_run) {
        const msg = if (sprite_name) |name|
            try std.fmt.allocPrint(alloc, "  [dry-run] sprite exec -s {s} bash -c \"{s}\"\n", .{ name, cmd })
        else
            try std.fmt.allocPrint(alloc, "  [dry-run] sprite exec bash -c \"{s}\"\n", .{cmd});
        defer alloc.free(msg);
        std.fs.File.stderr().writeAll(msg) catch {};
        return true;
    }

    var argv_list = std.ArrayList([]const u8).init(alloc);
    defer argv_list.deinit();
    try argv_list.append("sprite");
    try argv_list.append("exec");
    if (sprite_name) |name| {
        try argv_list.append("-s");
        try argv_list.append(name);
    }
    try argv_list.append("bash");
    try argv_list.append("-c");
    try argv_list.append(cmd);

    return run(alloc, argv_list.items);
}

/// Capture output from a bash command on a sprite.
pub fn sxCapture(alloc: std.mem.Allocator, sprite_name: ?[]const u8, cmd: []const u8) !?[]const u8 {
    var argv_list = std.ArrayList([]const u8).init(alloc);
    defer argv_list.deinit();
    try argv_list.append("sprite");
    try argv_list.append("exec");
    if (sprite_name) |name| {
        try argv_list.append("-s");
        try argv_list.append(name);
    }
    try argv_list.append("bash");
    try argv_list.append("-c");
    try argv_list.append(cmd);

    return runCaptureQuiet(alloc, argv_list.items);
}

/// Push a local file to a sprite.
pub fn pushFile(alloc: std.mem.Allocator, sprite_name: []const u8, local_path: []const u8, remote_path: []const u8, dry_run: bool) !void {
    if (dry_run) {
        const msg = try std.fmt.allocPrint(alloc, "  [dry-run] push {s} -> sprite:{s}\n", .{ local_path, remote_path });
        defer alloc.free(msg);
        std.fs.File.stderr().writeAll(msg) catch {};
        return;
    }

    // mkdir -p on remote
    const remote_dir = std.fs.path.dirname(remote_path) orelse "/";
    const mkdir_cmd = try std.fmt.allocPrint(alloc, "mkdir -p '{s}'", .{remote_dir});
    defer alloc.free(mkdir_cmd);
    _ = try sx(alloc, sprite_name, mkdir_cmd, false);

    // cat > remote_path, piping local file to stdin
    const cat_cmd = try std.fmt.allocPrint(alloc, "cat > '{s}'", .{remote_path});
    defer alloc.free(cat_cmd);

    const argv = &[_][]const u8{ "sprite", "exec", "-s", sprite_name, "bash", "-c", cat_cmd };
    var child = std.process.Child.init(argv, alloc);
    child.stderr_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stdin_behavior = .Pipe;
    try child.spawn();

    // Open local file and pipe its contents to the child's stdin
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
    if (!exitedOk(term)) {
        return error.PushFileFailed;
    }
}

/// Push a local directory to a sprite via tar pipe.
pub fn pushDir(alloc: std.mem.Allocator, sprite_name: []const u8, local_path: []const u8, remote_path: []const u8, dry_run: bool) !void {
    if (dry_run) {
        const msg = try std.fmt.allocPrint(alloc, "  [dry-run] push dir {s} -> sprite:{s}\n", .{ local_path, remote_path });
        defer alloc.free(msg);
        std.fs.File.stderr().writeAll(msg) catch {};
        return;
    }

    // mkdir -p on remote
    const mkdir_cmd = try std.fmt.allocPrint(alloc, "mkdir -p '{s}'", .{remote_path});
    defer alloc.free(mkdir_cmd);
    _ = try sx(alloc, sprite_name, mkdir_cmd, false);

    // Get dirname and basename of local path
    const dir_name = std.fs.path.dirname(local_path) orelse ".";
    const base_name = std.fs.path.basename(local_path);

    // Compute the parent of remote_path for tar extraction
    const remote_parent = std.fs.path.dirname(remote_path) orelse "/";

    // tar on local side
    const tar_argv = &[_][]const u8{ "tar", "czf", "-", "-C", dir_name, base_name };
    var tar_child = std.process.Child.init(tar_argv, alloc);
    tar_child.stderr_behavior = .Inherit;
    tar_child.stdout_behavior = .Pipe;
    tar_child.stdin_behavior = .Inherit;
    try tar_child.spawn();

    // sprite exec to untar on remote
    const untar_cmd = try std.fmt.allocPrint(alloc, "tar xzf - -C '{s}'", .{remote_parent});
    defer alloc.free(untar_cmd);
    const sprite_argv = &[_][]const u8{ "sprite", "exec", "-s", sprite_name, "bash", "-c", untar_cmd };
    var sprite_child = std.process.Child.init(sprite_argv, alloc);
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
    if (!exitedOk(term)) {
        return error.PushDirFailed;
    }
}

/// Check if the sprite CLI is installed.
pub fn isSpriteInstalled(alloc: std.mem.Allocator) bool {
    const argv = &[_][]const u8{ "sprite", "version" };
    var child = std.process.Child.init(argv, alloc);
    child.stderr_behavior = .Ignore;
    child.stdout_behavior = .Ignore;
    child.stdin_behavior = .Inherit;
    child.spawn() catch return false;
    const term = child.wait() catch return false;
    return exitedOk(term);
}
