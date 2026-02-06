use anyhow::{Context, Result};
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Stdio};

/// Configuration loaded from environment variables and .env file.
pub struct Config {
    pub sprite_token: String,
    pub agent: String,
    pub claude_auth: String,
    pub anthropic_api_key: String,
    pub model: String,
    pub checkpoint_interval: u64,
    pub env_file: String,
}

impl Config {
    /// Load configuration from environment variables, with .env file fallback.
    /// Hand-rolls the .env parser: skips comments (#), blank lines, parses KEY=VALUE,
    /// strips surrounding quotes from values.
    pub fn load() -> Result<Self> {
        let env_file = std::env::var("ENV_FILE").unwrap_or_else(|_| "./.env".to_string());

        // Parse .env file if present
        let mut env_vars: HashMap<String, String> = HashMap::new();
        if Path::new(&env_file).is_file() {
            let contents = fs::read_to_string(&env_file)
                .with_context(|| format!("Failed to read env file: {}", env_file))?;
            for line in contents.lines() {
                let trimmed = line.trim();
                // Skip empty lines and comments
                if trimmed.is_empty() || trimmed.starts_with('#') {
                    continue;
                }
                // Parse KEY=VALUE
                if let Some(eq_pos) = trimmed.find('=') {
                    let key = trimmed[..eq_pos].trim().to_string();
                    let mut value = trimmed[eq_pos + 1..].trim().to_string();
                    // Strip surrounding quotes (single or double)
                    if (value.starts_with('"') && value.ends_with('"'))
                        || (value.starts_with('\'') && value.ends_with('\''))
                    {
                        value = value[1..value.len() - 1].to_string();
                    }
                    env_vars.insert(key, value);
                }
            }
        }

        // Helper: get value from process env, falling back to .env vars
        let get = |key: &str| -> String {
            std::env::var(key).unwrap_or_else(|_| env_vars.get(key).cloned().unwrap_or_default())
        };

        let mut sprite_token = get("SPRITE_TOKEN");
        // Fallback: SPRITES_TOKEN from .env
        if sprite_token.is_empty() {
            sprite_token = get("SPRITES_TOKEN");
        }

        let checkpoint_interval: u64 = get("CHECKPOINT_INTERVAL").parse().unwrap_or(300);

        Ok(Config {
            sprite_token,
            agent: {
                let v = get("AGENT");
                if v.is_empty() {
                    "opencode".to_string()
                } else {
                    v
                }
            },
            claude_auth: {
                let v = get("CLAUDE_AUTH");
                if v.is_empty() {
                    "subscription".to_string()
                } else {
                    v
                }
            },
            anthropic_api_key: get("ANTHROPIC_API_KEY"),
            model: get("MODEL"),
            checkpoint_interval,
            env_file,
        })
    }
}

/// Wrapper for running commands on a sprite via `sprite exec`.
pub struct SpriteExec {
    pub sprite: String,
    pub dry_run: bool,
}

impl SpriteExec {
    pub fn new(sprite: &str, dry_run: bool) -> Self {
        SpriteExec {
            sprite: sprite.to_string(),
            dry_run,
        }
    }

    /// Run a bash command on the sprite. Returns stdout as a string.
    pub fn sx(&self, cmd: &str) -> Result<String> {
        if self.dry_run {
            println!(
                "  [dry-run] sprite exec -s {} bash -c {:?}",
                self.sprite, cmd
            );
            return Ok(String::new());
        }
        let output = Command::new("sprite")
            .args(["exec", "-s", &self.sprite, "bash", "-c", cmd])
            .output()
            .with_context(|| format!("Failed to run sprite exec: {}", cmd))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!(
                "sprite exec failed (exit {}): {}\nstderr: {}",
                output.status,
                cmd,
                stderr
            );
        }
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    /// Run a bash command on the sprite, ignoring errors. Returns stdout.
    pub fn sx_quiet(&self, cmd: &str) -> String {
        if self.dry_run {
            println!(
                "  [dry-run] sprite exec -s {} bash -c {:?}",
                self.sprite, cmd
            );
            return String::new();
        }
        let output = Command::new("sprite")
            .args(["exec", "-s", &self.sprite, "bash", "-c", cmd])
            .stderr(Stdio::null())
            .output();
        match output {
            Ok(o) => String::from_utf8_lossy(&o.stdout).trim().to_string(),
            Err(_) => String::new(),
        }
    }

    /// Run a bash command on the sprite interactively (inheriting stdio).
    pub fn sx_interactive(&self, cmd: &str) -> Result<()> {
        if self.dry_run {
            println!(
                "  [dry-run] sprite exec -s {} bash -c {:?}",
                self.sprite, cmd
            );
            return Ok(());
        }
        let status = Command::new("sprite")
            .args(["exec", "-s", &self.sprite, "bash", "-c", cmd])
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .with_context(|| format!("Failed to run sprite exec: {}", cmd))?;
        if !status.success() {
            anyhow::bail!("sprite exec failed (exit {}): {}", status, cmd);
        }
        Ok(())
    }

    /// Push a local file to a remote path on the sprite.
    pub fn push_file(&self, src: &str, dest: &str) -> Result<()> {
        if self.dry_run {
            println!("  [dry-run] push {} -> sprite:{}", src, dest);
            return Ok(());
        }
        let dest_dir = Path::new(dest)
            .parent()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|| "/".to_string());
        self.sx(&format!("mkdir -p '{}'", dest_dir))?;

        let file_content =
            fs::read(src).with_context(|| format!("Failed to read local file: {}", src))?;
        let mut child = Command::new("sprite")
            .args([
                "exec",
                "-s",
                &self.sprite,
                "bash",
                "-c",
                &format!("cat > '{}'", dest),
            ])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to spawn sprite exec for file push")?;

        if let Some(ref mut stdin) = child.stdin {
            stdin
                .write_all(&file_content)
                .context("Failed to write file content to sprite exec stdin")?;
        }
        // Drop stdin to signal EOF
        drop(child.stdin.take());

        let output = child
            .wait_with_output()
            .context("Failed to wait for sprite exec")?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("push_file failed: {}", stderr);
        }
        Ok(())
    }

    /// Push a local directory to a remote path on the sprite via tar piping.
    pub fn push_dir(&self, src: &str, dest: &str) -> Result<()> {
        if self.dry_run {
            println!("  [dry-run] push dir {} -> sprite:{}", src, dest);
            return Ok(());
        }

        let src_path = Path::new(src);
        let parent = src_path
            .parent()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|| ".".to_string());
        let base = src_path
            .file_name()
            .map(|f| f.to_string_lossy().to_string())
            .unwrap_or_else(|| ".".to_string());
        let dest_parent = Path::new(dest)
            .parent()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|| "/".to_string());

        self.sx(&format!("mkdir -p '{}'", dest))?;

        // Spawn local tar to create archive
        let mut tar_proc = Command::new("tar")
            .args(["czf", "-", "-C", &parent, &base])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to spawn local tar")?;

        // Spawn sprite exec to receive and extract archive
        let mut sprite_proc = Command::new("sprite")
            .args([
                "exec",
                "-s",
                &self.sprite,
                "bash",
                "-c",
                &format!("tar xzf - -C '{}'", dest_parent),
            ])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .context("Failed to spawn sprite exec for dir push")?;

        // Pipe tar stdout -> sprite stdin in a thread
        let tar_stdout = tar_proc.stdout.take().context("No tar stdout")?;
        let sprite_stdin = sprite_proc.stdin.take().context("No sprite stdin")?;

        let copy_thread = std::thread::spawn(move || -> Result<()> {
            let mut reader = tar_stdout;
            let mut writer = sprite_stdin;
            io::copy(&mut reader, &mut writer).context("Failed to pipe tar to sprite")?;
            Ok(())
        });

        copy_thread
            .join()
            .map_err(|_| anyhow::anyhow!("Copy thread panicked"))??;

        let tar_output = tar_proc
            .wait_with_output()
            .context("Failed to wait for tar")?;
        if !tar_output.status.success() {
            let stderr = String::from_utf8_lossy(&tar_output.stderr);
            anyhow::bail!("Local tar failed: {}", stderr);
        }

        let sprite_output = sprite_proc
            .wait_with_output()
            .context("Failed to wait for sprite exec")?;
        if !sprite_output.status.success() {
            let stderr = String::from_utf8_lossy(&sprite_output.stderr);
            anyhow::bail!("Remote tar extract failed: {}", stderr);
        }

        Ok(())
    }
}

/// Run a sprite CLI command directly (not via exec). Returns stdout.
pub fn sprite_cmd(args: &[&str]) -> Result<String> {
    let output = Command::new("sprite")
        .args(args)
        .output()
        .with_context(|| format!("Failed to run sprite {}", args.join(" ")))?;
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

/// Check if a command exists on PATH.
pub fn command_exists(name: &str) -> bool {
    Command::new("which")
        .arg(name)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}
