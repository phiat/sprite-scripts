use anyhow::{bail, Context, Result};
use std::fs;
use std::io;
use std::path::Path;
use std::process::{Command, Stdio};

/// Run the pull subcommand: pull a file or directory from a sprite.
pub fn run(remote_path: String, local_path: String, sprite_name: Option<String>) -> Result<()> {
    let mut sprite_args: Vec<String> = vec!["exec".to_string()];
    if let Some(ref name) = sprite_name {
        sprite_args.push("-s".to_string());
        sprite_args.push(name.clone());
    }

    // Check if remote path is a directory or file
    let is_dir = check_remote_is_dir(&remote_path, &sprite_args)?;

    if is_dir {
        println!("Pulling directory: {} -> {}", remote_path, local_path);
        pull_directory(&remote_path, &local_path, &sprite_args)?;
    } else {
        println!("Pulling file: {} -> {}", remote_path, local_path);
        pull_file(&remote_path, &local_path, &sprite_args)?;
    }

    println!("Done.");
    Ok(())
}

/// Check if a remote path is a directory by running a test on the sprite.
fn check_remote_is_dir(remote_path: &str, sprite_args: &[String]) -> Result<bool> {
    let bash_cmd = format!(
        "[ -d '{}' ] && echo dir || echo file",
        remote_path
    );
    let mut args = sprite_args.to_vec();
    args.extend(["bash".to_string(), "-c".to_string(), bash_cmd]);

    let output = Command::new("sprite")
        .args(&args)
        .output()
        .context("Failed to check remote path type")?;

    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Ok(stdout == "dir")
}

/// Pull a directory by tar-piping: sprite exec tar czf -> local tar xzf
fn pull_directory(remote_path: &str, local_path: &str, sprite_args: &[String]) -> Result<()> {
    // Ensure local directory exists
    fs::create_dir_all(local_path)
        .with_context(|| format!("Failed to create local directory: {}", local_path))?;

    // Spawn sprite exec to create tar archive
    let mut remote_args = sprite_args.to_vec();
    remote_args.extend([
        "tar".to_string(),
        "czf".to_string(),
        "-".to_string(),
        "-C".to_string(),
        remote_path.to_string(),
        ".".to_string(),
    ]);

    let mut sprite_proc = Command::new("sprite")
        .args(&remote_args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to spawn sprite exec for directory pull")?;

    // Spawn local tar to extract
    let mut tar_proc = Command::new("tar")
        .args(["xzf", "-", "-C", local_path])
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to spawn local tar for extraction")?;

    // Pipe sprite stdout -> tar stdin
    let sprite_stdout = sprite_proc.stdout.take().context("No sprite stdout")?;
    let tar_stdin = tar_proc.stdin.take().context("No tar stdin")?;

    let copy_thread = std::thread::spawn(move || -> Result<()> {
        let mut reader = sprite_stdout;
        let mut writer = tar_stdin;
        io::copy(&mut reader, &mut writer).context("Failed to pipe sprite to tar")?;
        Ok(())
    });

    copy_thread
        .join()
        .map_err(|_| anyhow::anyhow!("Copy thread panicked"))??;

    let sprite_output = sprite_proc
        .wait_with_output()
        .context("Failed to wait for sprite exec")?;
    if !sprite_output.status.success() {
        let stderr = String::from_utf8_lossy(&sprite_output.stderr);
        bail!("Remote tar failed: {}", stderr);
    }

    let tar_output = tar_proc
        .wait_with_output()
        .context("Failed to wait for local tar")?;
    if !tar_output.status.success() {
        let stderr = String::from_utf8_lossy(&tar_output.stderr);
        bail!("Local tar extract failed: {}", stderr);
    }

    Ok(())
}

/// Pull a single file by capturing sprite exec cat output.
fn pull_file(remote_path: &str, local_path: &str, sprite_args: &[String]) -> Result<()> {
    // Ensure parent directory exists locally
    if let Some(parent) = Path::new(local_path).parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create local directory: {}", parent.display()))?;
    }

    let mut args = sprite_args.to_vec();
    args.extend(["cat".to_string(), remote_path.to_string()]);

    let output = Command::new("sprite")
        .args(&args)
        .output()
        .context("Failed to run sprite exec cat")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("Failed to pull file: {}", stderr);
    }

    fs::write(local_path, &output.stdout)
        .with_context(|| format!("Failed to write local file: {}", local_path))?;

    Ok(())
}
