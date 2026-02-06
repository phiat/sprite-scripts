using System;
using System.IO;

namespace SpriteTool;

/// <summary>
/// All configuration derived from environment variables and .env files.
/// </summary>
public class Config
{
    public string SpriteToken { get; set; } = "";
    public string Agent { get; set; } = "opencode";
    public string ClaudeAuth { get; set; } = "subscription";
    public string AnthropicApiKey { get; set; } = "";
    public string Model { get; set; } = "";
    public int CheckpointInterval { get; set; } = 300;
    public string EnvFile { get; set; } = "./.env";

    /// <summary>
    /// Hand-rolled .env parser. Skips blank lines and comments.
    /// Parses KEY=VALUE with optional single/double quote stripping.
    /// Sets parsed values into Environment (does not overwrite existing).
    /// </summary>
    public static void ParseEnvFile(string path)
    {
        if (!File.Exists(path))
            return;

        foreach (var rawLine in File.ReadAllLines(path))
        {
            var line = rawLine.Trim();

            // Skip blank lines and comments
            if (string.IsNullOrEmpty(line) || line.StartsWith('#'))
                continue;

            // Must contain =
            var eqIndex = line.IndexOf('=');
            if (eqIndex < 0)
                continue;

            var key = line[..eqIndex].Trim();
            var value = line[(eqIndex + 1)..].Trim();

            // Strip matching quotes
            if (value.Length >= 2 &&
                value[0] == value[^1] &&
                (value[0] == '"' || value[0] == '\''))
            {
                value = value[1..^1];
            }

            // Set in environment (don't overwrite existing)
            if (string.IsNullOrEmpty(Environment.GetEnvironmentVariable(key)))
            {
                Environment.SetEnvironmentVariable(key, value);
            }
        }
    }

    /// <summary>
    /// Load configuration from .env file and environment variables.
    /// </summary>
    public static Config Load()
    {
        var envFile = Environment.GetEnvironmentVariable("ENV_FILE") ?? "./.env";

        // Parse .env file first (populates environment for keys not already set)
        ParseEnvFile(envFile);

        // SPRITE_TOKEN with SPRITES_TOKEN fallback
        var spriteToken = Environment.GetEnvironmentVariable("SPRITE_TOKEN") ?? "";
        if (string.IsNullOrEmpty(spriteToken))
            spriteToken = Environment.GetEnvironmentVariable("SPRITES_TOKEN") ?? "";

        // Checkpoint interval
        var intervalStr = Environment.GetEnvironmentVariable("CHECKPOINT_INTERVAL") ?? "300";
        if (!int.TryParse(intervalStr, out var checkpointInterval))
        {
            Console.Error.WriteLine($"Error: invalid CHECKPOINT_INTERVAL '{intervalStr}' (must be integer)");
            Environment.Exit(1);
        }

        return new Config
        {
            SpriteToken = spriteToken,
            Agent = Environment.GetEnvironmentVariable("AGENT") ?? "opencode",
            ClaudeAuth = Environment.GetEnvironmentVariable("CLAUDE_AUTH") ?? "subscription",
            AnthropicApiKey = Environment.GetEnvironmentVariable("ANTHROPIC_API_KEY") ?? "",
            Model = Environment.GetEnvironmentVariable("MODEL") ?? "",
            CheckpointInterval = checkpointInterval,
            EnvFile = envFile,
        };
    }
}
