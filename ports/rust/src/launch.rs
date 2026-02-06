use anyhow::{bail, Context, Result};
use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use sprite_tool::{command_exists, sprite_cmd, Config, SpriteExec};

/// Run the launch subcommand.
pub fn run(
    dry_run: bool,
    no_checkpoint: bool,
    upload_dirs: Vec<String>,
    sprite_name: String,
    plan_file: Option<String>,
) -> Result<()> {
    let config = Config::load()?;
    let sx = SpriteExec::new(&sprite_name, dry_run);
    let checkpointing = !no_checkpoint;

    // 1. Check/install sprite CLI
    if !command_exists("sprite") {
        if dry_run {
            println!("  [dry-run] Would install sprite CLI");
        } else {
            println!("Installing sprite CLI...");
            let status = Command::new("sh")
                .args(["-c", "curl -fsSL https://sprites.dev/install.sh | sh"])
                .status()
                .context("Failed to install sprite CLI")?;
            if !status.success() {
                bail!("Sprite CLI installation failed");
            }
            // Update PATH for this process
            if let Ok(home) = std::env::var("HOME") {
                let current_path = std::env::var("PATH").unwrap_or_default();
                std::env::set_var("PATH", format!("{}/.local/bin:{}", home, current_path));
            }
        }
    }

    // 2. Auth sprite
    if !config.sprite_token.is_empty() {
        println!("Authenticating sprite with token...");
        if !dry_run {
            let status = Command::new("sprite")
                .args(["auth", "setup", "--token", &config.sprite_token])
                .status()
                .context("Failed to run sprite auth setup")?;
            if !status.success() {
                bail!("sprite auth setup failed");
            }
        }
    } else {
        println!("No SPRITE_TOKEN set. Running interactive login...");
        if !dry_run {
            let status = Command::new("sprite")
                .arg("login")
                .stdin(Stdio::inherit())
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .status()
                .context("Failed to run sprite login")?;
            if !status.success() {
                bail!("sprite login failed");
            }
        }
    }

    // 3. Create sprite (or use existing)
    if dry_run {
        println!("  [dry-run] Would create or reuse sprite '{}'", sprite_name);
    } else {
        let ls_output = sprite_cmd(&["ls"]).unwrap_or_default();
        let sprite_exists = ls_output
            .lines()
            .any(|line| line.split_whitespace().any(|word| word == sprite_name));
        if sprite_exists {
            println!("Sprite '{}' already exists, using it.", sprite_name);
        } else {
            println!("Creating sprite: {}", sprite_name);
            let status = Command::new("sprite")
                .args(["create", "-skip-console", &sprite_name])
                .status()
                .context("Failed to create sprite")?;
            if !status.success() {
                bail!("sprite create failed");
            }
        }
    }

    // 4. Push .env to sprite
    if Path::new(&config.env_file).is_file() {
        println!("Pushing {}...", config.env_file);
        sx.push_file(&config.env_file, "/home/sprite/.env")?;
    }

    // 5. Push plan file if provided
    if let Some(ref plan) = plan_file {
        if Path::new(plan).is_file() {
            println!("Pushing {}...", plan);
            sx.push_file(plan, "/home/sprite/plan.md")?;
        }
    }

    // 6. Upload directories if provided
    for dir in &upload_dirs {
        if Path::new(dir).is_dir() {
            let dirname = Path::new(dir)
                .file_name()
                .map(|f| f.to_string_lossy().to_string())
                .unwrap_or_else(|| "upload".to_string());
            let remote_dest = format!("/home/sprite/{}", dirname);
            println!("Uploading directory: {} -> {}", dir, remote_dest);
            sx.push_dir(dir, &remote_dest)?;
        } else {
            println!("WARNING: --upload dir '{}' not found, skipping.", dir);
        }
    }

    // 7. Setup git + beads
    println!("Initializing git...");
    sx.sx("cd /home/sprite && git init -b main 2>/dev/null || true")?;

    println!("Installing beads...");
    sx.sx("curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash")?;

    // 8. Install and auth coding agent
    match config.agent.as_str() {
        "claude" => {
            println!("Setting up claude...");
            sx.sx("command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code")?;

            if config.claude_auth == "subscription" {
                let home = std::env::var("HOME").unwrap_or_else(|_| "/root".to_string());
                let creds_path = format!("{}/.claude/.credentials.json", home);
                if Path::new(&creds_path).is_file() {
                    println!("Copying claude subscription credentials...");
                    sx.push_file(&creds_path, "/home/sprite/.claude/.credentials.json")?;
                    sx.sx("chmod 600 ~/.claude/.credentials.json")?;
                } else {
                    bail!(
                        "~/.claude/.credentials.json not found\n\
                         Run 'claude' locally first to authenticate, then re-run this script."
                    );
                }
            } else if config.claude_auth == "apikey" && !config.anthropic_api_key.is_empty() {
                println!("Setting ANTHROPIC_API_KEY in sprite...");
                sx.sx(&format!(
                    "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"{}\"' >> ~/.bashrc",
                    config.anthropic_api_key
                ))?;
            } else {
                bail!(
                    "No valid claude auth configured\n\
                     Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY"
                );
            }
        }
        "opencode" => {
            println!("Setting up opencode...");
            sx.sx("[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash")?;
            sx.sx("grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc")?;
        }
        other => {
            bail!("Unknown AGENT '{}'. Use 'claude' or 'opencode'.", other);
        }
    }

    // 9. Launch agent with plan (or open console)
    println!();
    println!("==========================================");
    println!("Sprite '{}' is ready!", sprite_name);
    if config.model.is_empty() {
        println!("Agent: {}", config.agent);
    } else {
        println!("Agent: {} (model: {})", config.agent, config.model);
    }
    if checkpointing {
        println!("Checkpointing: every {}s", config.checkpoint_interval);
    }
    println!("==========================================");

    if dry_run {
        println!();
        println!(
            "[dry-run] Would launch {} with plan. No changes were made.",
            config.agent
        );
        return Ok(());
    }

    if plan_file.is_some() {
        // Start auto-checkpointing before agent runs
        let stop_flag = Arc::new(AtomicBool::new(false));
        let checkpoint_handle = if checkpointing {
            let flag = Arc::clone(&stop_flag);
            let sprite_clone = sprite_name.clone();
            let interval = config.checkpoint_interval;
            Some(thread::spawn(move || {
                checkpoint_loop(&sprite_clone, interval, &flag);
            }))
        } else {
            None
        };

        println!("Launching {} with plan...", config.agent);

        let agent_result = match config.agent.as_str() {
            "claude" => {
                let model_flag = if config.model.is_empty() {
                    String::new()
                } else {
                    format!("--model {} ", config.model)
                };
                sx.sx_interactive(&format!(
                    "cd /home/sprite && claude {}-p 'read plan.md and complete the plan please'",
                    model_flag
                ))
            }
            "opencode" => {
                let oc_model = if config.model.is_empty() {
                    "opencode/big-pickle".to_string()
                } else {
                    config.model.clone()
                };
                sx.sx_interactive(&format!(
                    "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m {} 'read plan.md and complete the plan please'",
                    oc_model
                ))
            }
            _ => unreachable!(),
        };

        // Stop checkpointing
        stop_flag.store(true, Ordering::SeqCst);
        if let Some(handle) = checkpoint_handle {
            let _ = handle.join();
        }

        // Report agent result (non-fatal)
        if let Err(e) = agent_result {
            eprintln!("Agent exited with error: {}", e);
        }

        // Final checkpoint
        println!("Creating final checkpoint...");
        let cp_status = Command::new("sprite")
            .args(["checkpoint", "create", "-s", &sprite_name])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status();
        match cp_status {
            Ok(s) if s.success() => println!("Final checkpoint saved."),
            _ => println!("Final checkpoint failed (non-fatal)."),
        }
    } else {
        println!("Opening console...");
        let status = Command::new("sprite")
            .args(["console", "-s", &sprite_name])
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .context("Failed to open sprite console")?;
        if !status.success() {
            bail!("sprite console exited with error");
        }
    }

    Ok(())
}

/// Background checkpoint loop. Runs `sprite checkpoint create` every `interval` seconds
/// until `stop` is set to true.
fn checkpoint_loop(sprite: &str, interval: u64, stop: &AtomicBool) {
    println!(
        "Auto-checkpointing every {}s (background thread)",
        interval
    );
    loop {
        // Sleep in 1-second increments to check the stop flag promptly
        for _ in 0..interval {
            if stop.load(Ordering::SeqCst) {
                return;
            }
            thread::sleep(Duration::from_secs(1));
        }
        if stop.load(Ordering::SeqCst) {
            return;
        }

        let now = chrono_free_timestamp();
        println!("[checkpoint] Creating checkpoint at {}...", now);
        let result = Command::new("sprite")
            .args(["checkpoint", "create", "-s", sprite])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status();
        match result {
            Ok(s) if s.success() => println!("[checkpoint] Done."),
            _ => println!("[checkpoint] Failed (non-fatal)."),
        }
    }
}

/// Get a simple HH:MM:SS timestamp without pulling in chrono.
fn chrono_free_timestamp() -> String {
    // Use the `date` command as a simple cross-platform fallback
    let output = Command::new("date")
        .arg("+%H:%M:%S")
        .output();
    match output {
        Ok(o) => String::from_utf8_lossy(&o.stdout).trim().to_string(),
        Err(_) => "??:??:??".to_string(),
    }
}
