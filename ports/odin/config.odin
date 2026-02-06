package main

import "core:os"
import "core:strings"
import "core:strconv"
import "core:c"

foreign import posix_env "system:c"

foreign posix_env {
    @(link_name = "setenv")
    _setenv :: proc "c" (name: cstring, value: cstring, overwrite: c.int) -> c.int ---
}

// Configuration loaded from environment variables
Config :: struct {
    sprite_token:        string,
    agent:               string,
    claude_auth:         string,
    anthropic_api_key:   string,
    model:               string,
    checkpoint_interval: int,
    env_file:            string,
}

// Build a Config from current environment variables.
// Call this after load_env_file() has been applied.
build_config :: proc() -> Config {
    cfg := Config{
        sprite_token        = get_env_or("SPRITE_TOKEN", ""),
        agent               = get_env_or("AGENT", "opencode"),
        claude_auth         = get_env_or("CLAUDE_AUTH", "subscription"),
        anthropic_api_key   = get_env_or("ANTHROPIC_API_KEY", ""),
        model               = get_env_or("MODEL", ""),
        checkpoint_interval = 300,
        env_file            = get_env_or("ENV_FILE", "./.env"),
    }

    interval_str, interval_found := os.lookup_env("CHECKPOINT_INTERVAL")
    if interval_found && interval_str != "" {
        val, ok := strconv.parse_int(interval_str)
        if ok {
            cfg.checkpoint_interval = val
        }
    }

    return cfg
}

// Load a .env file: skip blank lines and #comments, parse KEY=VALUE,
// strip surrounding quotes from values, and set them in the process environment.
load_env_file :: proc(path: string) -> bool {
    data, ok := os.read_entire_file(path)
    if !ok {
        return false
    }

    content := string(data)
    lines := strings.split(content, "\n")

    for line in lines {
        trimmed := strings.trim_space(line)

        // Skip blank and comment lines
        if len(trimmed) == 0 {
            continue
        }
        if trimmed[0] == '#' {
            continue
        }

        // Find '='
        eq_idx := strings.index(trimmed, "=")
        if eq_idx < 0 {
            continue
        }

        key := strings.trim_space(trimmed[:eq_idx])
        val := strings.trim_space(trimmed[eq_idx + 1:])

        // Strip surrounding quotes (single or double)
        if len(val) >= 2 {
            if (val[0] == '"' && val[len(val) - 1] == '"') ||
               (val[0] == '\'' && val[len(val) - 1] == '\'') {
                val = val[1:len(val) - 1]
            }
        }

        // Set in process environment
        set_env(key, val)
    }

    return true
}

// Apply SPRITES_TOKEN -> SPRITE_TOKEN fallback after loading .env
apply_sprite_token_fallback :: proc() {
    sprite_token, st_found := os.lookup_env("SPRITE_TOKEN")
    if !st_found || sprite_token == "" {
        sprites_token, sts_found := os.lookup_env("SPRITES_TOKEN")
        if sts_found && sprites_token != "" {
            set_env("SPRITE_TOKEN", sprites_token)
        }
    }
}

// Helper: get environment variable with a default fallback
get_env_or :: proc(key: string, default_val: string) -> string {
    val, found := os.lookup_env(key)
    if !found || val == "" {
        return default_val
    }
    return val
}

// Helper: set an environment variable using POSIX setenv
set_env :: proc(key: string, value: string) {
    k := strings.clone_to_cstring(key)
    v := strings.clone_to_cstring(value)
    _setenv(k, v, 1)
}
