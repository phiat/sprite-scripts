module SpriteTool.Config

open System
open System.IO

/// Configuration derived from environment variables and .env file.
type Config =
    { SpriteToken: string
      Agent: string
      ClaudeAuth: string
      AnthropicApiKey: string
      Model: string
      CheckpointInterval: int
      EnvFile: string }

/// Parse a .env file: skip blank lines and #comments, split on first '=',
/// strip surrounding quotes, set into Environment (don't overwrite existing).
let parseEnvFile (path: string) =
    if not (File.Exists path) then
        Map.empty
    else
        File.ReadAllLines path
        |> Array.fold
            (fun acc line ->
                let trimmed = line.Trim()

                if String.IsNullOrEmpty trimmed || trimmed.StartsWith "#" then
                    acc
                else
                    match trimmed.IndexOf '=' with
                    | -1 -> acc
                    | idx ->
                        let key = trimmed.Substring(0, idx).Trim()
                        let raw = trimmed.Substring(idx + 1).Trim()

                        let value =
                            if
                                raw.Length >= 2
                                && (raw.[0] = '"' || raw.[0] = '\'')
                                && raw.[0] = raw.[raw.Length - 1]
                            then
                                raw.Substring(1, raw.Length - 2)
                            else
                                raw

                        // Set in environment if not already present
                        if Environment.GetEnvironmentVariable key |> isNull then
                            Environment.SetEnvironmentVariable(key, value)

                        acc |> Map.add key value)
            Map.empty

/// Helper: get env var with fallback default.
let private envOr (name: string) (fallback: string) =
    match Environment.GetEnvironmentVariable name with
    | null -> fallback
    | "" -> fallback
    | v -> v

/// Load configuration from .env file and environment variables.
let load () =
    let envFile = envOr "ENV_FILE" "./.env"

    // Parse .env first so its values become available via Environment
    parseEnvFile envFile |> ignore

    // SPRITE_TOKEN with SPRITES_TOKEN fallback
    let spriteToken =
        match envOr "SPRITE_TOKEN" "" with
        | "" -> envOr "SPRITES_TOKEN" ""
        | t -> t

    let intervalStr = envOr "CHECKPOINT_INTERVAL" "300"

    let checkpointInterval =
        match Int32.TryParse intervalStr with
        | true, v -> v
        | false, _ ->
            eprintfn "Error: invalid CHECKPOINT_INTERVAL '%s' (must be integer)" intervalStr
            exit 1

    { SpriteToken = spriteToken
      Agent = envOr "AGENT" "opencode"
      ClaudeAuth = envOr "CLAUDE_AUTH" "subscription"
      AnthropicApiKey = envOr "ANTHROPIC_API_KEY" ""
      Model = envOr "MODEL" ""
      CheckpointInterval = checkpointInterval
      EnvFile = envFile }
