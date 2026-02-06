module config;

import std.conv : to;
import std.file : exists, readText;
import std.process : environment;
import std.stdio : stderr;
import std.string : strip, indexOf, startsWith, splitLines;

/// Configuration derived from environment variables and .env file.
struct Config
{
    string spriteToken;
    string agent;
    string claudeAuth;
    string anthropicApiKey;
    string model;
    int checkpointInterval;
    string envFile;
}

/// Get environment variable with fallback default.
private string envOr(string name, string fallback)
{
    string val = environment.get(name, "");
    if (val.length == 0)
        return fallback;
    return val;
}

/// Parse a .env file: skip blank lines and #comments, split on first '=',
/// strip surrounding quotes, set into environment (don't overwrite existing).
void parseEnvFile(string path)
{
    if (!exists(path))
        return;

    string content = readText(path);
    foreach (line; content.splitLines())
    {
        string trimmed = line.strip();
        if (trimmed.length == 0 || trimmed[0] == '#')
            continue;

        auto idx = trimmed.indexOf('=');
        if (idx < 0)
            continue;

        string key = trimmed[0 .. idx].strip();
        string raw = trimmed[idx + 1 .. $].strip();

        // Strip surrounding quotes
        string value = raw;
        if (raw.length >= 2
            && (raw[0] == '"' || raw[0] == '\'')
            && raw[0] == raw[$ - 1])
        {
            value = raw[1 .. $ - 1];
        }

        // Set in environment if not already present
        string existing = environment.get(key, "");
        if (existing.length == 0)
        {
            environment[key] = value;
        }
    }
}

/// Load configuration from .env file and environment variables.
Config loadConfig()
{
    string envFile = envOr("ENV_FILE", "./.env");

    // Parse .env first so its values become available via environment
    parseEnvFile(envFile);

    // SPRITE_TOKEN with SPRITES_TOKEN fallback
    string spriteToken = envOr("SPRITE_TOKEN", "");
    if (spriteToken.length == 0)
        spriteToken = envOr("SPRITES_TOKEN", "");

    string intervalStr = envOr("CHECKPOINT_INTERVAL", "300");
    int checkpointInterval;
    try
    {
        checkpointInterval = to!int(intervalStr);
    }
    catch (Exception e)
    {
        stderr.writefln("Error: invalid CHECKPOINT_INTERVAL '%s' (must be integer)", intervalStr);
        import core.stdc.stdlib : exit;
        exit(1);
    }

    return Config(
        spriteToken,
        envOr("AGENT", "opencode"),
        envOr("CLAUDE_AUTH", "subscription"),
        envOr("ANTHROPIC_API_KEY", ""),
        envOr("MODEL", ""),
        checkpointInterval,
        envFile,
    );
}
