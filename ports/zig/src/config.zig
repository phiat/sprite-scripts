const std = @import("std");

pub const Config = struct {
    sprite_token: ?[]const u8 = null,
    agent: []const u8 = "opencode",
    claude_auth: []const u8 = "subscription",
    anthropic_api_key: ?[]const u8 = null,
    model: ?[]const u8 = null,
    checkpoint_interval: u64 = 300,
    env_file: []const u8 = "./.env",
};

/// Load a .env file: skip blank lines and #comments, parse KEY=VALUE, strip
/// surrounding quotes from values. Returns owned slices allocated with `alloc`.
pub fn loadEnvFile(alloc: std.mem.Allocator, path: []const u8) !std.StringHashMap([]const u8) {
    var map = std.StringHashMap([]const u8).init(alloc);

    const file = std.fs.cwd().openFile(path, .{}) catch |err| switch (err) {
        error.FileNotFound => return map,
        else => return err,
    };
    defer file.close();

    // Read the entire file into memory
    const contents = file.readToEndAlloc(alloc, 1024 * 1024) catch |err| {
        _ = err;
        return map;
    };
    defer alloc.free(contents);

    // Split by newlines and parse each line
    var lines = std.mem.splitScalar(u8, contents, '\n');
    while (lines.next()) |raw_line| {
        // Trim trailing \r for Windows-style line endings
        const line = std.mem.trimRight(u8, raw_line, "\r");

        // Strip leading/trailing whitespace
        const trimmed = std.mem.trim(u8, line, " \t");

        // Skip blank and comment lines
        if (trimmed.len == 0) continue;
        if (trimmed[0] == '#') continue;

        // Find '='
        const eq_idx = std.mem.indexOfScalar(u8, trimmed, '=') orelse continue;

        const key_raw = std.mem.trim(u8, trimmed[0..eq_idx], " \t");
        var val_raw = std.mem.trim(u8, trimmed[eq_idx + 1 ..], " \t");

        // Strip surrounding quotes (single or double)
        if (val_raw.len >= 2) {
            if ((val_raw[0] == '"' and val_raw[val_raw.len - 1] == '"') or
                (val_raw[0] == '\'' and val_raw[val_raw.len - 1] == '\''))
            {
                val_raw = val_raw[1 .. val_raw.len - 1];
            }
        }

        const key = try alloc.dupe(u8, key_raw);
        const val = try alloc.dupe(u8, val_raw);
        try map.put(key, val);
    }

    return map;
}

/// Get a config value: first check real environment, then the .env map.
fn getConfigVal(env_map: *const std.StringHashMap([]const u8), key: []const u8) ?[]const u8 {
    // Real environment takes precedence
    if (std.posix.getenv(key)) |v| return v;
    // Then .env file
    return env_map.get(key);
}

/// Build a Config struct from environment variables and .env file contents.
pub fn buildConfig(env_map: *const std.StringHashMap([]const u8)) Config {
    var cfg = Config{};

    // SPRITE_TOKEN with SPRITES_TOKEN fallback from .env
    if (getConfigVal(env_map, "SPRITE_TOKEN")) |v| {
        cfg.sprite_token = v;
    } else if (env_map.get("SPRITES_TOKEN")) |v| {
        // Fallback: SPRITES_TOKEN in .env -> SPRITE_TOKEN
        cfg.sprite_token = v;
    }

    if (getConfigVal(env_map, "AGENT")) |v| {
        cfg.agent = v;
    }
    if (getConfigVal(env_map, "CLAUDE_AUTH")) |v| {
        cfg.claude_auth = v;
    }
    if (getConfigVal(env_map, "ANTHROPIC_API_KEY")) |v| {
        cfg.anthropic_api_key = v;
    }
    if (getConfigVal(env_map, "MODEL")) |v| {
        cfg.model = v;
    }
    if (getConfigVal(env_map, "CHECKPOINT_INTERVAL")) |v| {
        cfg.checkpoint_interval = std.fmt.parseInt(u64, v, 10) catch 300;
    }
    if (getConfigVal(env_map, "ENV_FILE")) |v| {
        cfg.env_file = v;
    }

    return cfg;
}
