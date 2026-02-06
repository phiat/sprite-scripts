/// sprite-tool launch: Create and configure a sprite with coding agent, git, beads.

import config.{type Config, Config}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import sprite

/// Print usage information and halt.
fn usage() -> Nil {
  io.println(
    "Usage: sprite-tool launch [options] <sprite-name> [plan-file]

Options:
  --dry-run              Show what would happen without executing
  --no-checkpoint        Disable auto-checkpointing
  --upload <dir>         Upload a local directory to /home/sprite/<dirname>
                         (repeatable: --upload ./data --upload ./tests)

Environment variables:
  ENV_FILE               Path to .env file (default: ./.env)
  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)
  AGENT                  \"opencode\" (default) or \"claude\"
  CLAUDE_AUTH            \"subscription\" (default) or \"apikey\"
  MODEL                  Model override (see below)
  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)

Model examples:
  OpenCode: MODEL=opencode/big-pickle  (free, default)
            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)
            MODEL=openai/gpt-4o
            MODEL=google/gemini-2.5-pro
  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku

Examples:
  sprite-tool launch my-project plan.md
  sprite-tool launch --upload ./data my-project plan.md
  sprite-tool launch --upload ./data --upload ./tests my-project plan.md
  sprite-tool launch --dry-run my-project plan.md
  MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md
  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md",
  )
  halt(1)
}

@external(erlang, "erlang", "halt")
fn halt(code: Int) -> Nil

/// Parsed launch arguments.
pub type LaunchArgs {
  LaunchArgs(
    dry_run: Bool,
    checkpointing: Bool,
    upload_dirs: List(String),
    sprite_name: String,
    plan_file: String,
  )
}

/// Parse CLI arguments, returning LaunchArgs or calling usage/halt on errors.
fn parse_args(args: List(String)) -> LaunchArgs {
  parse_args_loop(args, False, True, [], [])
}

fn parse_args_loop(
  args: List(String),
  dry_run: Bool,
  checkpointing: Bool,
  upload_dirs: List(String),
  positional: List(String),
) -> LaunchArgs {
  case args {
    [] -> finish_parse(dry_run, checkpointing, upload_dirs, positional)
    ["--dry-run", ..rest] ->
      parse_args_loop(rest, True, checkpointing, upload_dirs, positional)
    ["--no-checkpoint", ..rest] ->
      parse_args_loop(rest, dry_run, False, upload_dirs, positional)
    ["--upload", dir, ..rest] ->
      parse_args_loop(
        rest,
        dry_run,
        checkpointing,
        list.append(upload_dirs, [dir]),
        positional,
      )
    ["--upload"] -> {
      io.println("Error: --upload requires an argument")
      halt(1)
      // Gleam needs a value here even though halt never returns
      parse_args_loop([], dry_run, checkpointing, upload_dirs, positional)
    }
    ["--help", ..] -> {
      usage()
      parse_args_loop([], dry_run, checkpointing, upload_dirs, positional)
    }
    ["-h", ..] -> {
      usage()
      parse_args_loop([], dry_run, checkpointing, upload_dirs, positional)
    }
    [arg, ..rest] ->
      case string.starts_with(arg, "--") {
        True -> {
          io.println("Unknown option: " <> arg)
          usage()
          parse_args_loop(rest, dry_run, checkpointing, upload_dirs, positional)
        }
        False ->
          parse_args_loop(
            rest,
            dry_run,
            checkpointing,
            upload_dirs,
            list.append(positional, [arg]),
          )
      }
  }
}

fn finish_parse(
  dry_run: Bool,
  checkpointing: Bool,
  upload_dirs: List(String),
  positional: List(String),
) -> LaunchArgs {
  case positional {
    [] -> {
      usage()
      // unreachable after halt, but satisfies type checker
      LaunchArgs(
        dry_run: dry_run,
        checkpointing: checkpointing,
        upload_dirs: upload_dirs,
        sprite_name: "",
        plan_file: "",
      )
    }
    [sprite_name] ->
      LaunchArgs(
        dry_run: dry_run,
        checkpointing: checkpointing,
        upload_dirs: upload_dirs,
        sprite_name: sprite_name,
        plan_file: "",
      )
    [sprite_name, plan_file, ..] ->
      LaunchArgs(
        dry_run: dry_run,
        checkpointing: checkpointing,
        upload_dirs: upload_dirs,
        sprite_name: sprite_name,
        plan_file: plan_file,
      )
  }
}

/// Message type for checkpoint process.
pub type CheckpointMsg {
  Stop
}

/// Start the background checkpoint loop as an Erlang process.
/// Returns the Subject to send Stop to.
fn start_checkpointing(
  sprite_name: String,
  interval: Int,
) -> process.Subject(CheckpointMsg) {
  let subject = process.new_subject()
  let _pid =
    process.spawn(fn() { checkpoint_loop(subject, sprite_name, interval) })
  io.println(
    "Auto-checkpointing every " <> int.to_string(interval) <> "s (process)",
  )
  subject
}

fn checkpoint_loop(
  subject: process.Subject(CheckpointMsg),
  sprite_name: String,
  interval: Int,
) -> Nil {
  // Use process.receive with timeout (interval in ms)
  // If we receive Stop, exit. If timeout, checkpoint and loop.
  case process.receive(subject, interval * 1000) {
    Ok(Stop) -> Nil
    Error(_) -> {
      // Timeout - time to checkpoint
      let now = sprite.time_now()
      io.println("[checkpoint] Creating checkpoint at " <> now <> "...")
      let result =
        sprite.exec_status(
          "sprite checkpoint create -s '"
          <> sprite_name
          <> "' 2>/dev/null",
        )
      case result {
        0 -> io.println("[checkpoint] Done.")
        _ -> io.println("[checkpoint] Failed (non-fatal).")
      }
      checkpoint_loop(subject, sprite_name, interval)
    }
  }
}

/// Stop the checkpoint process.
fn stop_checkpointing(subject: process.Subject(CheckpointMsg)) -> Nil {
  process.send(subject, Stop)
}

/// Execute the launch subcommand.
pub fn run(args: List(String)) -> Nil {
  // Load config from .env + environment
  let cfg = config.load()

  // Parse flags
  let parsed = parse_args(args)

  let cfg =
    Config(
      ..cfg,
      dry_run: parsed.dry_run,
      checkpointing: parsed.checkpointing,
      upload_dirs: parsed.upload_dirs,
    )

  let sprite_name = parsed.sprite_name
  let plan_file = parsed.plan_file
  let dry_run = cfg.dry_run

  // Helper closures
  let do_sx = fn(cmd) { sprite.sx(sprite_name, cmd, dry_run) }
  let do_sx_pass = fn(cmd) {
    sprite.sx_passthrough(sprite_name, cmd, dry_run)
  }
  let do_push_file = fn(src, dest) {
    sprite.push_file(sprite_name, src, dest, dry_run)
  }
  let do_push_dir = fn(src, dest) {
    sprite.push_dir(sprite_name, src, dest, dry_run)
  }

  // 1. Check/install sprite CLI
  case sprite.command_exists("sprite") {
    True -> Nil
    False ->
      case dry_run {
        True -> {
          io.println("  [dry-run] Would install sprite CLI")
          Nil
        }
        False -> {
          io.println("Installing sprite CLI...")
          let _ =
            sprite.exec_status(
              "curl -fsSL https://sprites.dev/install.sh | sh",
            )
          Nil
        }
      }
  }

  // 2. Auth sprite (non-interactive if token provided)
  case cfg.sprite_token {
    "" -> {
      io.println("No SPRITE_TOKEN set. Running interactive login...")
      case dry_run {
        True -> Nil
        False -> {
          let _ = sprite.exec_status("sprite login")
          Nil
        }
      }
    }
    token -> {
      io.println("Authenticating sprite with token...")
      case dry_run {
        True -> Nil
        False -> {
          let _ =
            sprite.exec_status("sprite auth setup --token '" <> token <> "'")
          Nil
        }
      }
    }
  }

  // 3. Create sprite (or use existing)
  case dry_run {
    True -> {
      io.println(
        "  [dry-run] Would create or reuse sprite '" <> sprite_name <> "'",
      )
      Nil
    }
    False ->
      case sprite.sprite_exists(sprite_name) {
        True -> {
          io.println(
            "Sprite '" <> sprite_name <> "' already exists, using it.",
          )
          Nil
        }
        False -> {
          io.println("Creating sprite: " <> sprite_name)
          let _ =
            sprite.exec_status(
              "sprite create -skip-console '" <> sprite_name <> "'",
            )
          Nil
        }
      }
  }

  // 4. Push .env to sprite
  case sprite.file_exists(cfg.env_file) {
    True -> {
      io.println("Pushing " <> cfg.env_file <> "...")
      do_push_file(cfg.env_file, "/home/sprite/.env")
    }
    False -> Nil
  }

  // 5. Push plan file if provided
  case plan_file {
    "" -> Nil
    pf ->
      case sprite.file_exists(pf) {
        True -> {
          io.println("Pushing " <> pf <> "...")
          do_push_file(pf, "/home/sprite/plan.md")
        }
        False -> Nil
      }
  }

  // 6. Upload directories if provided
  list.each(cfg.upload_dirs, fn(dir) {
    case sprite.is_directory(dir) {
      True -> {
        let dir_name = sprite.basename(dir)
        io.println(
          "Uploading directory: "
          <> dir
          <> " -> /home/sprite/"
          <> dir_name,
        )
        do_push_dir(dir, "/home/sprite/" <> dir_name)
      }
      False -> {
        io.println(
          "WARNING: --upload dir '" <> dir <> "' not found, skipping.",
        )
        Nil
      }
    }
  })

  // 7. Setup git + beads
  io.println("Initializing git...")
  let _ = do_sx("cd /home/sprite && git init -b main 2>/dev/null || true")

  io.println("Installing beads...")
  let _ =
    do_sx(
      "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash",
    )

  // 8. Install and auth coding agent
  setup_agent(cfg, sprite_name, do_sx, do_push_file)

  // 9. Launch agent with plan (or open console)
  io.println("")
  io.println("==========================================")
  io.println("Sprite '" <> sprite_name <> "' is ready!")
  let model_note = case cfg.model {
    "" -> ""
    m -> " (model: " <> m <> ")"
  }
  io.println("Agent: " <> cfg.agent <> model_note)
  case cfg.checkpointing {
    True ->
      io.println(
        "Checkpointing: every " <> int.to_string(cfg.checkpoint_interval) <> "s",
      )
    False -> Nil
  }
  io.println("==========================================")

  case dry_run {
    True -> {
      io.println("")
      io.println(
        "[dry-run] Would launch "
        <> cfg.agent
        <> " with plan. No changes were made.",
      )
      Nil
    }
    False -> launch_or_console(cfg, sprite_name, plan_file, do_sx_pass)
  }
}

fn setup_agent(
  cfg: Config,
  _sprite_name: String,
  do_sx: fn(String) -> String,
  do_push_file: fn(String, String) -> Nil,
) -> Nil {
  case cfg.agent {
    "claude" -> {
      io.println("Setting up claude...")
      let _ =
        do_sx(
          "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code",
        )

      case cfg.claude_auth {
        "subscription" -> {
          let home = sprite.home_dir()
          let creds_path = home <> "/.claude/.credentials.json"
          case sprite.file_exists(creds_path) {
            True -> {
              io.println("Copying claude subscription credentials...")
              do_push_file(
                creds_path,
                "/home/sprite/.claude/.credentials.json",
              )
              let _ = do_sx("chmod 600 ~/.claude/.credentials.json")
              Nil
            }
            False -> {
              io.println("ERROR: ~/.claude/.credentials.json not found")
              io.println(
                "Run 'claude' locally first to authenticate, then re-run this script.",
              )
              halt(1)
            }
          }
        }
        "apikey" ->
          case cfg.anthropic_api_key {
            "" -> {
              io.println("ERROR: No valid claude auth configured")
              io.println(
                "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY",
              )
              halt(1)
            }
            key -> {
              io.println("Setting ANTHROPIC_API_KEY in sprite...")
              let _ =
                do_sx(
                  "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\""
                  <> key
                  <> "\"' >> ~/.bashrc",
                )
              Nil
            }
          }
        _ -> {
          io.println("ERROR: No valid claude auth configured")
          io.println(
            "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY",
          )
          halt(1)
        }
      }
    }
    "opencode" -> {
      io.println("Setting up opencode...")
      let _ =
        do_sx(
          "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash",
        )
      let _ =
        do_sx(
          "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc",
        )
      Nil
    }
    agent -> {
      io.println(
        "ERROR: Unknown AGENT '" <> agent <> "'. Use 'claude' or 'opencode'.",
      )
      halt(1)
    }
  }
}

fn launch_or_console(
  cfg: Config,
  sprite_name: String,
  plan_file: String,
  do_sx_pass: fn(String) -> Int,
) -> Nil {
  case plan_file {
    "" -> {
      io.println("Opening console...")
      let _ = sprite.exec_status("sprite console -s '" <> sprite_name <> "'")
      Nil
    }
    _ -> {
      // Start auto-checkpointing before agent runs
      let checkpoint_subject = case cfg.checkpointing {
        True -> Ok(start_checkpointing(sprite_name, cfg.checkpoint_interval))
        False -> Error(Nil)
      }

      io.println("Launching " <> cfg.agent <> " with plan...")

      case cfg.agent {
        "claude" -> {
          let model_flag = case cfg.model {
            "" -> ""
            m -> "--model " <> m <> " "
          }
          let _ =
            do_sx_pass(
              "cd /home/sprite && claude "
              <> model_flag
              <> "-p 'read plan.md and complete the plan please'",
            )
          Nil
        }
        "opencode" -> {
          let oc_model = case cfg.model {
            "" -> "opencode/big-pickle"
            m -> m
          }
          let _ =
            do_sx_pass(
              "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m "
              <> oc_model
              <> " 'read plan.md and complete the plan please'",
            )
          Nil
        }
        _ -> Nil
      }

      // Stop checkpointing
      case checkpoint_subject {
        Ok(subj) -> stop_checkpointing(subj)
        Error(_) -> Nil
      }

      // Final checkpoint
      io.println("Creating final checkpoint...")
      let result =
        sprite.exec_status(
          "sprite checkpoint create -s '"
          <> sprite_name
          <> "' 2>/dev/null",
        )
      case result {
        0 -> io.println("Final checkpoint saved.")
        _ -> io.println("Final checkpoint failed (non-fatal).")
      }
      Nil
    }
  }
}
