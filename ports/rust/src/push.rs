use anyhow::{bail, Context, Result};
use std::fs;
use std::io;
use std::path::Path;
use std::process::{Command, Stdio};

/// Run the push subcommand: push a local file or directory to a sprite.
pub fn run(local_path: String, remote_path: String, sprite_name: Option<String>) -> Result<()> {
    let local = Path::new(&local_path);
    if !local.exists() {
        bail!("Error: {} does not exist", local_path);
    }

    let mut sprite_args: Vec<String> = vec!["exec".to_string()];
    if let Some(ref name) = sprite_name {
        sprite_args.push("-s".to_string());
        sprite_args.push(name.clone());
    }

    if local.is_dir() {
        println!("Pushing directory: {} -> {}", local_path, remote_path);
        push_directory(&local_path, &remote_path, &sprite_args)?;
    } else {
        println!("Pushing file: {} -> {}", local_path, remote_path);
        push_file(&local_path, &remote_path, &sprite_args)?;
    }

    println!("Done.");
    Ok(())
}

/// Push a directory by tar-piping: local tar czf -> sprite exec tar xzf
fn push_directory(local_path: &str, remote_path: &str, sprite_args: &[String]) -> Result<()> {
    let src_path = Path::new(local_path);
    let parent = src_path
        .parent()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|| ".".to_string());
    let base = src_path
        .file_name()
        .map(|f| f.to_string_lossy().to_string())
        .unwrap_or_else(|| ".".to_string());

    // Spawn local tar
    let mut tar_proc = Command::new("tar")
        .args(["czf", "-", "-C", &parent, &base])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to spawn local tar")?;

    // Spawn sprite exec to receive archive
    let bash_cmd = format!(
        "mkdir -p '{}' && tar xzf - -C '{}' --strip-components=1",
        remote_path, remote_path
    );
    let mut args = sprite_args.to_vec();
    args.extend(["bash".to_string(), "-c".to_string(), bash_cmd]);

    let mut sprite_proc = Command::new("sprite")
        .args(&args)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to spawn sprite exec for directory push")?;

    // Pipe tar stdout -> sprite stdin
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
        bail!("Local tar failed: {}", stderr);
    }

    let sprite_output = sprite_proc
        .wait_with_output()
        .context("Failed to wait for sprite exec")?;
    if !sprite_output.status.success() {
        let stderr = String::from_utf8_lossy(&sprite_output.stderr);
        bail!("Remote tar extract failed: {}", stderr);
    }

    Ok(())
}

/// Push a single file by piping its content to sprite exec cat.
fn push_file(local_path: &str, remote_path: &str, sprite_args: &[String]) -> Result<()> {
    let remote_dir = Path::new(remote_path)
        .parent()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|| "/".to_string());

    let bash_cmd = format!("mkdir -p '{}' && cat > '{}'", remote_dir, remote_path);
    let mut args = sprite_args.to_vec();
    args.extend(["bash".to_string(), "-c".to_string(), bash_cmd]);

    let file_content =
        fs::read(local_path).with_context(|| format!("Failed to read: {}", local_path))?;

    let mut child = Command::new("sprite")
        .args(&args)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .context("Failed to spawn sprite exec for file push")?;

    if let Some(ref mut stdin) = child.stdin {
        use std::io::Write;
        stdin
            .write_all(&file_content)
            .context("Failed to write file content to sprite stdin")?;
    }
    drop(child.stdin.take());

    let output = child
        .wait_with_output()
        .context("Failed to wait for sprite exec")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("File push failed: {}", stderr);
    }

    Ok(())
}
