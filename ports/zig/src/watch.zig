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
        \\Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]
        \\
        \\Arguments:
        \\  sprite-name     Name of the sprite to watch
        \\  task-id         Beads task ID to track (default: auto-detect first open critical task)
        \\  poll-interval   Seconds between polls (default: 30)
        \\
        \\Examples:
        \\  sprite-tool watch ember-red-hawk
        \\  sprite-tool watch ember-red-hawk CRM-1
        \\  sprite-tool watch ember-red-hawk CRM-1 60
        \\
    );
}

pub fn execute(alloc: std.mem.Allocator, raw_args: []const []const u8) !void {
    if (raw_args.len < 1) {
        printUsage();
        std.process.exit(1);
    }

    const sprite_name = raw_args[0];
    var task_id: ?[]const u8 = if (raw_args.len >= 2) raw_args[1] else null;
    const poll_interval: u64 = if (raw_args.len >= 3)
        std.fmt.parseInt(u64, raw_args[2], 10) catch 30
    else
        30;

    // Owned copy of task_id if auto-detected (so we can free it later)
    var owned_task_id: ?[]const u8 = null;
    defer if (owned_task_id) |tid| alloc.free(tid);

    // Auto-detect task if not specified
    if (task_id == null) {
        printMsg("Detecting tracker task...\n");

        // Try critical tasks first
        const critical_output = try sprite.sxCapture(alloc, sprite_name, "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'");
        if (critical_output) |output| {
            if (output.len > 0) {
                owned_task_id = output;
                task_id = output;
            } else {
                alloc.free(output);
            }
        }

        // Fallback to first open task
        if (task_id == null) {
            printMsg("No critical task found. Falling back to first open task...\n");
            const open_output = try sprite.sxCapture(alloc, sprite_name, "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'");
            if (open_output) |output| {
                if (output.len > 0) {
                    owned_task_id = output;
                    task_id = output;
                } else {
                    alloc.free(output);
                }
            }
        }

        if (task_id == null) {
            const msg = try std.fmt.allocPrint(alloc, "ERROR: No beads tasks found on sprite '{s}'\nSpecify a task ID manually: sprite-tool watch {s} <task-id>\n", .{ sprite_name, sprite_name });
            defer alloc.free(msg);
            printErr(msg);
            std.process.exit(1);
        }

        const msg = try std.fmt.allocPrint(alloc, "Tracking task: {s}\n", .{task_id.?});
        defer alloc.free(msg);
        printMsg(msg);
    }

    const tid = task_id.?;

    {
        const msg = try std.fmt.allocPrint(alloc, "Watching sprite '{s}' task '{s}' (every {d}s)\nPress Ctrl+C to stop\n\n", .{ sprite_name, tid, poll_interval });
        defer alloc.free(msg);
        printMsg(msg);
    }

    const sleep_ns = poll_interval * std.time.ns_per_s;

    while (true) {
        // Clear screen
        printMsg("\x1b[2J\x1b[H");

        {
            const msg = try std.fmt.allocPrint(alloc, "=== sprite-watch: {s} / {s} ===\n\n", .{ sprite_name, tid });
            defer alloc.free(msg);
            printMsg(msg);
        }

        // Show task status
        const show_cmd = try std.fmt.allocPrint(alloc, "cd /home/sprite && bd show {s} 2>/dev/null", .{tid});
        defer alloc.free(show_cmd);
        const show_output = try sprite.sxCapture(alloc, sprite_name, show_cmd);
        if (show_output) |output| {
            defer alloc.free(output);
            printMsg(output);
            printMsg("\n\n");
        } else {
            printMsg("(could not read task)\n\n");
        }

        // Show recent comments
        printMsg("--- Recent updates ---\n");
        const comments_cmd = try std.fmt.allocPrint(alloc, "cd /home/sprite && bd comments {s} 2>/dev/null | tail -8", .{tid});
        defer alloc.free(comments_cmd);
        const comments_output = try sprite.sxCapture(alloc, sprite_name, comments_cmd);
        if (comments_output) |output| {
            defer alloc.free(output);
            printMsg(output);
            printMsg("\n\n");
        } else {
            printMsg("(no comments)\n\n");
        }

        // Check if done
        const status_cmd = try std.fmt.allocPrint(alloc, "cd /home/sprite && bd show {s} 2>/dev/null | grep -i status", .{tid});
        defer alloc.free(status_cmd);
        const status_output = try sprite.sxCapture(alloc, sprite_name, status_cmd);
        if (status_output) |status| {
            defer alloc.free(status);
            // Check for closed/done/completed (case-insensitive by converting to lower)
            const lower = try std.ascii.allocLowerString(alloc, status);
            defer alloc.free(lower);
            if (std.mem.indexOf(u8, lower, "closed") != null or
                std.mem.indexOf(u8, lower, "done") != null or
                std.mem.indexOf(u8, lower, "completed") != null)
            {
                printMsg("==========================================\n");
                printMsg("PROJECT COMPLETE\n");
                printMsg("==========================================\n");
                break;
            }
        }

        std.Thread.sleep(sleep_ns);
    }
}
