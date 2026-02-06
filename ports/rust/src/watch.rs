use anyhow::{bail, Result};
use std::thread;
use std::time::Duration;

use sprite_tool::SpriteExec;

/// Run the watch subcommand: poll beads task for progress.
pub fn run(sprite_name: String, task_id: Option<String>, poll_interval: Option<u64>) -> Result<()> {
    let interval = poll_interval.unwrap_or(30);
    let sx = SpriteExec::new(&sprite_name, false);

    // Auto-detect task if not specified
    let task_id = match task_id {
        Some(id) if !id.is_empty() => id,
        _ => auto_detect_task(&sx, &sprite_name)?,
    };

    println!(
        "Watching sprite '{}' task '{}' (every {}s)",
        sprite_name, task_id, interval
    );
    println!("Press Ctrl+C to stop");
    println!();

    loop {
        // Clear screen using ANSI escape codes
        print!("\x1b[2J\x1b[H");

        // Get current time
        let time_str = get_time();
        println!(
            "=== sprite-watch: {} / {} === {} ===",
            sprite_name, task_id, time_str
        );
        println!();

        // Show task status
        let status_output = sx.sx_quiet(&format!(
            "cd /home/sprite && bd show {} 2>/dev/null",
            task_id
        ));
        if status_output.is_empty() {
            println!("(could not read task)");
        } else {
            println!("{}", status_output);
        }
        println!();

        // Show recent comments
        println!("--- Recent updates ---");
        let comments_output = sx.sx_quiet(&format!(
            "cd /home/sprite && bd comments {} 2>/dev/null | tail -8",
            task_id
        ));
        if comments_output.is_empty() {
            println!("(no comments)");
        } else {
            println!("{}", comments_output);
        }
        println!();

        // Check if done
        let status_line = sx.sx_quiet(&format!(
            "cd /home/sprite && bd show {} 2>/dev/null | grep -i status",
            task_id
        ));
        let status_lower = status_line.to_lowercase();
        if status_lower.contains("closed")
            || status_lower.contains("done")
            || status_lower.contains("completed")
        {
            println!("==========================================");
            println!("PROJECT COMPLETE");
            println!("==========================================");
            break;
        }

        thread::sleep(Duration::from_secs(interval));
    }

    Ok(())
}

/// Auto-detect the beads task ID by querying the sprite.
fn auto_detect_task(sx: &SpriteExec, sprite_name: &str) -> Result<String> {
    println!("Detecting tracker task...");

    // Try critical priority first
    let task_id = sx.sx_quiet(
        "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'",
    );
    if !task_id.is_empty() {
        println!("Tracking task: {}", task_id);
        return Ok(task_id);
    }

    // Fallback to first open task
    println!("No critical task found. Falling back to first open task...");
    let task_id = sx.sx_quiet(
        "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'",
    );
    if !task_id.is_empty() {
        println!("Tracking task: {}", task_id);
        return Ok(task_id);
    }

    bail!(
        "No beads tasks found on sprite '{}'\n\
         Specify a task ID manually: sprite-tool watch {} <task-id>",
        sprite_name,
        sprite_name
    );
}

/// Get a simple HH:MM:SS timestamp.
fn get_time() -> String {
    let output = std::process::Command::new("date")
        .arg("+%H:%M:%S")
        .output();
    match output {
        Ok(o) => String::from_utf8_lossy(&o.stdout).trim().to_string(),
        Err(_) => "??:??:??".to_string(),
    }
}
