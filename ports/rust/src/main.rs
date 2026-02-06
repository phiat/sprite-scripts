use anyhow::Result;
use clap::{Parser, Subcommand};

mod launch;
mod pull;
mod push;
mod watch;

/// CLI tool for managing sprites with coding agents, git, and beads.
#[derive(Parser)]
#[command(name = "sprite-tool", version, about)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Create and configure a sprite with coding agent, git, beads
    Launch {
        /// Show what would happen without executing
        #[arg(long)]
        dry_run: bool,

        /// Disable auto-checkpointing
        #[arg(long)]
        no_checkpoint: bool,

        /// Upload a local directory to /home/sprite/<dirname> (repeatable)
        #[arg(long = "upload", value_name = "DIR")]
        upload_dirs: Vec<String>,

        /// Name of the sprite to create or reuse
        sprite_name: String,

        /// Path to a plan file (optional)
        plan_file: Option<String>,
    },

    /// Push local file or directory to a sprite
    Push {
        /// Local path (file or directory)
        local_path: String,

        /// Remote path on the sprite
        remote_path: String,

        /// Sprite name (optional, uses default if omitted)
        sprite_name: Option<String>,
    },

    /// Pull file or directory from a sprite
    Pull {
        /// Remote path on the sprite
        remote_path: String,

        /// Local path to write to
        local_path: String,

        /// Sprite name (optional, uses default if omitted)
        sprite_name: Option<String>,
    },

    /// Poll beads task for progress
    Watch {
        /// Name of the sprite to watch
        sprite_name: String,

        /// Beads task ID to track (default: auto-detect first open critical task)
        task_id: Option<String>,

        /// Seconds between polls (default: 30)
        poll_interval: Option<u64>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Launch {
            dry_run,
            no_checkpoint,
            upload_dirs,
            sprite_name,
            plan_file,
        } => launch::run(dry_run, no_checkpoint, upload_dirs, sprite_name, plan_file),

        Commands::Push {
            local_path,
            remote_path,
            sprite_name,
        } => push::run(local_path, remote_path, sprite_name),

        Commands::Pull {
            remote_path,
            local_path,
            sprite_name,
        } => pull::run(remote_path, local_path, sprite_name),

        Commands::Watch {
            sprite_name,
            task_id,
            poll_interval,
        } => watch::run(sprite_name, task_id, poll_interval),
    }
}
