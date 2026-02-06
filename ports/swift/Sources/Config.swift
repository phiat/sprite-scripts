import Foundation

/// Configuration loaded from environment variables and .env file.
struct Config {
    let spriteToken: String
    let agent: String
    let claudeAuth: String
    let anthropicApiKey: String
    let model: String
    let checkpointInterval: Int
    let envFile: String

    /// Load configuration from environment variables, with .env file fallback.
    /// Hand-rolls the .env parser: skips comments (#), blank lines, parses KEY=VALUE,
    /// strips surrounding quotes from values.
    static func load() -> Config {
        let envFile = ProcessInfo.processInfo.environment["ENV_FILE"] ?? "./.env"

        // Parse .env file if present
        var envVars: [String: String] = [:]
        if FileManager.default.fileExists(atPath: envFile) {
            if let contents = try? String(contentsOfFile: envFile, encoding: .utf8) {
                for line in contents.components(separatedBy: .newlines) {
                    let trimmed = line.trimmingCharacters(in: .whitespaces)
                    // Skip empty lines and comments
                    if trimmed.isEmpty || trimmed.hasPrefix("#") {
                        continue
                    }
                    // Parse KEY=VALUE
                    if let eqRange = trimmed.range(of: "=") {
                        let key = String(trimmed[trimmed.startIndex..<eqRange.lowerBound])
                            .trimmingCharacters(in: .whitespaces)
                        var value = String(trimmed[eqRange.upperBound...])
                            .trimmingCharacters(in: .whitespaces)
                        // Strip surrounding quotes (single or double)
                        if (value.hasPrefix("\"") && value.hasSuffix("\""))
                            || (value.hasPrefix("'") && value.hasSuffix("'"))
                        {
                            value = String(value.dropFirst().dropLast())
                        }
                        envVars[key] = value
                    }
                }
            }
        }

        // Helper: get value from process env, falling back to .env vars
        func get(_ key: String) -> String {
            return ProcessInfo.processInfo.environment[key] ?? envVars[key] ?? ""
        }

        var spriteToken = get("SPRITE_TOKEN")
        // Fallback: SPRITES_TOKEN from .env
        if spriteToken.isEmpty {
            spriteToken = get("SPRITES_TOKEN")
        }

        let checkpointInterval = Int(get("CHECKPOINT_INTERVAL")) ?? 300

        let agentVal = get("AGENT")
        let claudeAuthVal = get("CLAUDE_AUTH")

        return Config(
            spriteToken: spriteToken,
            agent: agentVal.isEmpty ? "opencode" : agentVal,
            claudeAuth: claudeAuthVal.isEmpty ? "subscription" : claudeAuthVal,
            anthropicApiKey: get("ANTHROPIC_API_KEY"),
            model: get("MODEL"),
            checkpointInterval: checkpointInterval,
            envFile: envFile
        )
    }
}
