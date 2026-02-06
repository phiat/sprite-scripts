/// Configuration: .env parsing and environment variable loading.

import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

/// All configuration derived from environment variables and .env files.
pub type Config {
  Config(
    sprite_token: String,
    agent: String,
    claude_auth: String,
    anthropic_api_key: String,
    model: String,
    checkpoint_interval: Int,
    env_file: String,
    dry_run: Bool,
    checkpointing: Bool,
    upload_dirs: List(String),
  )
}

// --- Erlang FFI for os environment ---

@external(erlang, "config_ffi", "getenv_safe")
fn getenv_safe(key: String) -> Result(String, Nil)

@external(erlang, "config_ffi", "putenv_safe")
fn putenv_safe(key: String, value: String) -> Bool

/// Get an environment variable with a default fallback.
pub fn getenv(key: String, default: String) -> String {
  case getenv_safe(key) {
    Ok(val) ->
      case val {
        "" -> default
        _ -> val
      }
    Error(_) -> default
  }
}

/// Set an environment variable.
pub fn setenv(key: String, value: String) -> Nil {
  putenv_safe(key, value)
  Nil
}

/// Parse a .env file and set variables into the environment.
/// Skips blank lines and comments. Parses KEY=VALUE with optional quote stripping.
/// Does not overwrite existing environment variables.
pub fn parse_env_file(path: String) -> List(#(String, String)) {
  case simplifile.read(path) {
    Ok(contents) -> {
      contents
      |> string.split("\n")
      |> list.filter_map(fn(line) {
        let trimmed = string.trim(line)
        case trimmed {
          "" -> Error(Nil)
          "#" <> _ -> Error(Nil)
          _ ->
            case string.split_once(trimmed, "=") {
              Ok(#(key, value)) -> {
                let k = string.trim(key)
                let v = strip_quotes(string.trim(value))
                // Only set if not already in environment
                case getenv_safe(k) {
                  Error(_) -> setenv(k, v)
                  Ok("") -> setenv(k, v)
                  Ok(_) -> Nil
                }
                Ok(#(k, v))
              }
              Error(_) -> Error(Nil)
            }
        }
      })
    }
    Error(_) -> []
  }
}

/// Strip matching surrounding quotes (single or double) from a value.
fn strip_quotes(value: String) -> String {
  let len = string.length(value)
  case len >= 2 {
    True -> {
      let first = string.slice(value, 0, 1)
      let last = string.slice(value, len - 1, 1)
      case first == last && { first == "\"" || first == "'" } {
        True -> string.slice(value, 1, len - 2)
        False -> value
      }
    }
    False -> value
  }
}

/// Load configuration from .env file and environment variables.
pub fn load() -> Config {
  let env_file = getenv("ENV_FILE", "./.env")

  // Parse .env file first (populates env for keys not already set)
  let _ = parse_env_file(env_file)

  // SPRITE_TOKEN with SPRITES_TOKEN fallback
  let sprite_token = case getenv("SPRITE_TOKEN", "") {
    "" -> getenv("SPRITES_TOKEN", "")
    token -> token
  }

  let agent = getenv("AGENT", "opencode")
  let claude_auth = getenv("CLAUDE_AUTH", "subscription")
  let anthropic_api_key = getenv("ANTHROPIC_API_KEY", "")
  let model = getenv("MODEL", "")

  let checkpoint_interval = case
    int.parse(getenv("CHECKPOINT_INTERVAL", "300"))
  {
    Ok(n) -> n
    Error(_) -> {
      io.println(
        "Error: invalid CHECKPOINT_INTERVAL (must be integer), using default 300",
      )
      300
    }
  }

  Config(
    sprite_token: sprite_token,
    agent: agent,
    claude_auth: claude_auth,
    anthropic_api_key: anthropic_api_key,
    model: model,
    checkpoint_interval: checkpoint_interval,
    env_file: env_file,
    dry_run: False,
    checkpointing: True,
    upload_dirs: [],
  )
}
