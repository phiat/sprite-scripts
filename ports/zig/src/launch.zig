const std = @import("std");
const config_mod = @import("config.zig");
const sprite = @import("sprite.zig");

const Config = config_mod.Config;

pub const LaunchArgs = struct {
    dry_run: bool = false,
    no_checkpoint: bool = false,
    upload_dirs: std.ArrayList([]const u8) = .empty,
    sprite_name: ?[]const u8 = null,
    plan_file: ?[]const u8 = null,

    pub fn deinit(self: *LaunchArgs, alloc: std.mem.Allocator) void {
        self.upload_dirs.deinit(alloc);
    }
};

fn printMsg(msg: []const u8) void {
    std.fs.File.stdout().writeAll(msg) catch {};
}

fn printErr(msg: []const u8) void {
    std.fs.File.stderr().writeAll(msg) catch {};
}

pub fn parseArgs(alloc: std.mem.Allocator, raw_args: []const []const u8) !LaunchArgs {
    var args = LaunchArgs{};
    var i: usize = 0;
    while (i < raw_args.len) : (i += 1) {
        const arg = raw_args[i];
        if (std.mem.eql(u8, arg, "--dry-run")) {
            args.dry_run = true;
        } else if (std.mem.eql(u8, arg, "--no-checkpoint")) {
            args.no_checkpoint = true;
        } else if (std.mem.eql(u8, arg, "--upload")) {
            i += 1;
            if (i >= raw_args.len) {
                printErr("Error: --upload requires an argument\n");
                return error.InvalidArgs;
            }
            try args.upload_dirs.append(alloc, raw_args[i]);
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            std.process.exit(0);
        } else if (std.mem.startsWith(u8, arg, "--")) {
            const msg = std.fmt.allocPrint(alloc, "Unknown option: {s}\n", .{arg}) catch {
                printErr("Unknown option\n");
                std.process.exit(1);
            };
            defer alloc.free(msg);
            printErr(msg);
            printUsage();
            std.process.exit(1);
        } else {
            // Positional args: sprite-name [plan-file]
            if (args.sprite_name == null) {
                args.sprite_name = arg;
            } else if (args.plan_file == null) {
                args.plan_file = arg;
            } else {
                printErr("Too many positional arguments\n");
                printUsage();
                std.process.exit(1);
            }
        }
    }

    if (args.sprite_name == null) {
        printUsage();
        std.process.exit(1);
    }

    return args;
}

fn printUsage() void {
    printErr(
        \\Usage: sprite-tool launch [options] <sprite-name> [plan-file]
        \\
        \\Options:
        \\  --dry-run              Show what would happen without executing
        \\  --no-checkpoint        Disable auto-checkpointing
        \\  --upload <dir>         Upload a local directory to /home/sprite/<dirname>
        \\                         (repeatable: --upload ./data --upload ./tests)
        \\
        \\Environment variables:
        \\  ENV_FILE               Path to .env file (default: ./.env)
        \\  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)
        \\  AGENT                  "opencode" (default) or "claude"
        \\  CLAUDE_AUTH            "subscription" (default) or "apikey"
        \\  MODEL                  Model override
        \\  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)
        \\
    );
}

const CheckpointContext = struct {
    alloc: std.mem.Allocator,
    sprite_name: []const u8,
    interval_ns: u64,
    stop: *std.atomic.Value(bool),
};

fn checkpointThread(ctx: CheckpointContext) void {
    while (!ctx.stop.load(.acquire)) {
        std.Thread.sleep(ctx.interval_ns);
        if (ctx.stop.load(.acquire)) break;

        printMsg("[checkpoint] Creating checkpoint...\n");
        const argv = &[_][]const u8{ "sprite", "checkpoint", "create", "-s", ctx.sprite_name };
        var child = std.process.Child.init(argv, ctx.alloc);
        child.stderr_behavior = .Ignore;
        child.stdout_behavior = .Ignore;
        child.stdin_behavior = .Inherit;
        child.spawn() catch {
            printMsg("[checkpoint] Failed to spawn (non-fatal).\n");
            continue;
        };
        const term = child.wait() catch {
            printMsg("[checkpoint] Failed (non-fatal).\n");
            continue;
        };
        switch (term) {
            .Exited => |code| {
                if (code == 0) {
                    printMsg("[checkpoint] Done.\n");
                } else {
                    printMsg("[checkpoint] Failed (non-fatal).\n");
                }
            },
            else => {
                printMsg("[checkpoint] Failed (non-fatal).\n");
            },
        }
    }
}

pub fn execute(alloc: std.mem.Allocator, raw_args: []const []const u8) !void {
    var args = try parseArgs(alloc, raw_args);
    defer args.deinit(alloc);

    const sprite_name = args.sprite_name.?;

    // Determine env file path from ENV_FILE env var or default
    const env_file_path = std.posix.getenv("ENV_FILE") orelse "./.env";

    // Load .env
    var env_map = try config_mod.loadEnvFile(alloc, env_file_path);
    _ = &env_map; // keep alive
    const cfg = config_mod.buildConfig(&env_map);

    // 1. Check/install sprite CLI
    if (!sprite.isSpriteInstalled(alloc)) {
        if (args.dry_run) {
            printMsg("  [dry-run] Would install sprite CLI\n");
        } else {
            printMsg("Installing sprite CLI...\n");
            const install_argv = &[_][]const u8{ "bash", "-c", "curl -fsSL https://sprites.dev/install.sh | sh" };
            _ = try sprite.run(alloc, install_argv);
        }
    }

    // 2. Auth
    if (cfg.sprite_token) |token| {
        printMsg("Authenticating sprite with token...\n");
        if (!args.dry_run) {
            const auth_argv = &[_][]const u8{ "sprite", "auth", "setup", "--token", token };
            _ = try sprite.run(alloc, auth_argv);
        }
    } else {
        printMsg("No SPRITE_TOKEN set. Running interactive login...\n");
        if (!args.dry_run) {
            const login_argv = &[_][]const u8{ "sprite", "login" };
            _ = try sprite.run(alloc, login_argv);
        }
    }

    // 3. Create sprite (or use existing)
    if (args.dry_run) {
        const msg = try std.fmt.allocPrint(alloc, "  [dry-run] Would create or reuse sprite '{s}'\n", .{sprite_name});
        defer alloc.free(msg);
        printMsg(msg);
    } else {
        // Check if sprite already exists
        const ls_output = try sprite.runCaptureQuiet(alloc, &[_][]const u8{ "sprite", "ls" });
        var exists = false;
        if (ls_output) |output| {
            defer alloc.free(output);
            var lines = std.mem.splitScalar(u8, output, '\n');
            while (lines.next()) |line| {
                var words = std.mem.splitAny(u8, line, " \t");
                while (words.next()) |word| {
                    if (std.mem.eql(u8, word, sprite_name)) {
                        exists = true;
                        break;
                    }
                }
                if (exists) break;
            }
        }

        if (exists) {
            const msg = try std.fmt.allocPrint(alloc, "Sprite '{s}' already exists, using it.\n", .{sprite_name});
            defer alloc.free(msg);
            printMsg(msg);
        } else {
            const msg = try std.fmt.allocPrint(alloc, "Creating sprite: {s}\n", .{sprite_name});
            defer alloc.free(msg);
            printMsg(msg);
            const create_argv = &[_][]const u8{ "sprite", "create", "-skip-console", sprite_name };
            _ = try sprite.run(alloc, create_argv);
        }
    }

    // 4. Push .env to sprite
    if (fileExists(env_file_path)) {
        const msg = try std.fmt.allocPrint(alloc, "Pushing {s}...\n", .{env_file_path});
        defer alloc.free(msg);
        printMsg(msg);
        try sprite.pushFile(alloc, sprite_name, env_file_path, "/home/sprite/.env", args.dry_run);
    }

    // 5. Push plan file if provided
    if (args.plan_file) |plan_file| {
        if (fileExists(plan_file)) {
            const msg = try std.fmt.allocPrint(alloc, "Pushing {s}...\n", .{plan_file});
            defer alloc.free(msg);
            printMsg(msg);
            try sprite.pushFile(alloc, sprite_name, plan_file, "/home/sprite/plan.md", args.dry_run);
        }
    }

    // 6. Upload directories
    for (args.upload_dirs.items) |dir| {
        if (dirExists(dir)) {
            const basename = std.fs.path.basename(dir);
            const remote = try std.fmt.allocPrint(alloc, "/home/sprite/{s}", .{basename});
            defer alloc.free(remote);
            const msg = try std.fmt.allocPrint(alloc, "Uploading directory: {s} -> {s}\n", .{ dir, remote });
            defer alloc.free(msg);
            printMsg(msg);
            try sprite.pushDir(alloc, sprite_name, dir, remote, args.dry_run);
        } else {
            const msg = try std.fmt.allocPrint(alloc, "WARNING: --upload dir '{s}' not found, skipping.\n", .{dir});
            defer alloc.free(msg);
            printMsg(msg);
        }
    }

    // 7. Setup git + beads
    printMsg("Initializing git...\n");
    _ = try sprite.sx(alloc, sprite_name, "cd /home/sprite && git init -b main 2>/dev/null || true", args.dry_run);

    printMsg("Installing beads...\n");
    _ = try sprite.sx(alloc, sprite_name, "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash", args.dry_run);

    // 8. Install and auth coding agent
    if (std.mem.eql(u8, cfg.agent, "claude")) {
        printMsg("Setting up claude...\n");
        _ = try sprite.sx(alloc, sprite_name, "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code", args.dry_run);

        if (std.mem.eql(u8, cfg.claude_auth, "subscription")) {
            const home = std.posix.getenv("HOME") orelse "/root";
            const cred_path = try std.fmt.allocPrint(alloc, "{s}/.claude/.credentials.json", .{home});
            defer alloc.free(cred_path);
            if (fileExists(cred_path)) {
                printMsg("Copying claude subscription credentials...\n");
                try sprite.pushFile(alloc, sprite_name, cred_path, "/home/sprite/.claude/.credentials.json", args.dry_run);
                _ = try sprite.sx(alloc, sprite_name, "chmod 600 ~/.claude/.credentials.json", args.dry_run);
            } else {
                printErr("ERROR: ~/.claude/.credentials.json not found\n");
                printErr("Run 'claude' locally first to authenticate, then re-run this script.\n");
                std.process.exit(1);
            }
        } else if (std.mem.eql(u8, cfg.claude_auth, "apikey")) {
            if (cfg.anthropic_api_key) |api_key| {
                printMsg("Setting ANTHROPIC_API_KEY in sprite...\n");
                const cmd = try std.fmt.allocPrint(alloc, "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"{s}\"' >> ~/.bashrc", .{api_key});
                defer alloc.free(cmd);
                _ = try sprite.sx(alloc, sprite_name, cmd, args.dry_run);
            } else {
                printErr("ERROR: CLAUDE_AUTH=apikey but ANTHROPIC_API_KEY is not set\n");
                std.process.exit(1);
            }
        } else {
            printErr("ERROR: No valid claude auth configured\n");
            std.process.exit(1);
        }
    } else if (std.mem.eql(u8, cfg.agent, "opencode")) {
        printMsg("Setting up opencode...\n");
        _ = try sprite.sx(alloc, sprite_name, "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash", args.dry_run);
        _ = try sprite.sx(alloc, sprite_name, "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc", args.dry_run);
    } else {
        const msg = try std.fmt.allocPrint(alloc, "ERROR: Unknown AGENT '{s}'. Use 'claude' or 'opencode'.\n", .{cfg.agent});
        defer alloc.free(msg);
        printErr(msg);
        std.process.exit(1);
    }

    // 9. Launch agent with plan (or open console)
    printMsg("\n==========================================\n");
    {
        const msg = try std.fmt.allocPrint(alloc, "Sprite '{s}' is ready!\nAgent: {s}", .{ sprite_name, cfg.agent });
        defer alloc.free(msg);
        printMsg(msg);
    }
    if (cfg.model) |m| {
        const msg = try std.fmt.allocPrint(alloc, " (model: {s})", .{m});
        defer alloc.free(msg);
        printMsg(msg);
    }
    printMsg("\n");
    if (!args.no_checkpoint) {
        const msg = try std.fmt.allocPrint(alloc, "Checkpointing: every {d}s\n", .{cfg.checkpoint_interval});
        defer alloc.free(msg);
        printMsg(msg);
    }
    printMsg("==========================================\n");

    if (args.dry_run) {
        const msg = try std.fmt.allocPrint(alloc, "\n[dry-run] Would launch {s} with plan. No changes were made.\n", .{cfg.agent});
        defer alloc.free(msg);
        printMsg(msg);
        return;
    }

    if (args.plan_file != null) {
        // Start auto-checkpointing
        var stop_flag = std.atomic.Value(bool).init(false);
        var checkpoint_thread: ?std.Thread = null;

        if (!args.no_checkpoint) {
            const interval_ns = cfg.checkpoint_interval * std.time.ns_per_s;
            const ctx = CheckpointContext{
                .alloc = alloc,
                .sprite_name = sprite_name,
                .interval_ns = interval_ns,
                .stop = &stop_flag,
            };
            checkpoint_thread = try std.Thread.spawn(.{}, checkpointThread, .{ctx});
            const msg = try std.fmt.allocPrint(alloc, "Auto-checkpointing every {d}s\n", .{cfg.checkpoint_interval});
            defer alloc.free(msg);
            printMsg(msg);
        }

        {
            const msg = try std.fmt.allocPrint(alloc, "Launching {s} with plan...\n", .{cfg.agent});
            defer alloc.free(msg);
            printMsg(msg);
        }

        if (std.mem.eql(u8, cfg.agent, "claude")) {
            var cmd_buf: std.ArrayList(u8) = .empty;
            defer cmd_buf.deinit(alloc);
            try cmd_buf.appendSlice(alloc, "cd /home/sprite && claude ");
            if (cfg.model) |m| {
                try cmd_buf.appendSlice(alloc, "--model ");
                try cmd_buf.appendSlice(alloc, m);
                try cmd_buf.append(alloc, ' ');
            }
            try cmd_buf.appendSlice(alloc, "-p 'read plan.md and complete the plan please'");
            _ = try sprite.sx(alloc, sprite_name, cmd_buf.items, false);
        } else if (std.mem.eql(u8, cfg.agent, "opencode")) {
            const oc_model = cfg.model orelse "opencode/big-pickle";
            const cmd = try std.fmt.allocPrint(alloc, "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m {s} 'read plan.md and complete the plan please'", .{oc_model});
            defer alloc.free(cmd);
            _ = try sprite.sx(alloc, sprite_name, cmd, false);
        }

        // Stop checkpointing
        stop_flag.store(true, .release);
        if (checkpoint_thread) |t| {
            t.join();
        }

        // Final checkpoint
        printMsg("Creating final checkpoint...\n");
        const ck_argv = &[_][]const u8{ "sprite", "checkpoint", "create", "-s", sprite_name };
        if (try sprite.run(alloc, ck_argv)) {
            printMsg("Final checkpoint saved.\n");
        } else {
            printMsg("Final checkpoint failed (non-fatal).\n");
        }
    } else {
        printMsg("Opening console...\n");
        const console_argv = &[_][]const u8{ "sprite", "console", "-s", sprite_name };
        _ = try sprite.run(alloc, console_argv);
    }
}

fn fileExists(path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

fn dirExists(path: []const u8) bool {
    var dir = std.fs.cwd().openDir(path, .{}) catch return false;
    dir.close();
    return true;
}
