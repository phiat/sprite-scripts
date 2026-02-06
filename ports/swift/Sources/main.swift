import Foundation

// ============================================================
// sprite-tool: CLI tool for managing sprites
// Subcommands: launch, push, pull, watch
// ============================================================

/// Print usage information and exit.
func printUsage() -> Never {
    let usage = """
    Usage: sprite-tool <command> [options]

    Commands:
      launch    Create and configure a sprite with coding agent, git, beads
      push      Push local file or directory to a sprite
      pull      Pull file or directory from a sprite
      watch     Poll beads task for progress

    Run 'sprite-tool <command> --help' for command-specific help.
    """
    print(usage)
    exit(1)
}

func printLaunchUsage() -> Never {
    let usage = """
    Usage: sprite-tool launch [options] <sprite-name> [plan-file]

    Options:
      --dry-run              Show what would happen without executing
      --no-checkpoint        Disable auto-checkpointing
      --upload <dir>         Upload a local directory to /home/sprite/<dirname>
                             (repeatable: --upload ./data --upload ./tests)

    Environment variables:
      ENV_FILE               Path to .env file (default: ./.env)
      SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)
      AGENT                  "opencode" (default) or "claude"
      CLAUDE_AUTH            "subscription" (default) or "apikey"
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
    """
    print(usage)
    exit(1)
}

func printPushUsage() -> Never {
    let usage = """
    Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

    Examples:
      sprite-tool push ./file.txt /home/sprite/file.txt
      sprite-tool push ./mydir /home/sprite/mydir
      sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk
    """
    print(usage)
    exit(1)
}

func printPullUsage() -> Never {
    let usage = """
    Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

    Examples:
      sprite-tool pull /home/sprite/file.txt ./file.txt
      sprite-tool pull /home/sprite/mydir ./mydir
      sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk
    """
    print(usage)
    exit(1)
}

func printWatchUsage() -> Never {
    let usage = """
    Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

    Arguments:
      sprite-name     Name of the sprite to watch
      task-id         Beads task ID to track (default: auto-detect first open critical task)
      poll-interval   Seconds between polls (default: 30)

    Examples:
      sprite-tool watch ember-red-hawk
      sprite-tool watch ember-red-hawk CRM-1
      sprite-tool watch ember-red-hawk CRM-1 60
    """
    print(usage)
    exit(1)
}

// ============================================================
// Argument parsing and dispatch
// ============================================================

// CommandLine.arguments[0] is the executable path
let args = Array(CommandLine.arguments.dropFirst())

guard let subcommand = args.first else {
    printUsage()
}

let subArgs = Array(args.dropFirst())

switch subcommand {
case "launch":
    // Parse launch flags
    var dryRun = false
    var noCheckpoint = false
    var uploadDirs: [String] = []
    var positional: [String] = []
    var i = 0

    while i < subArgs.count {
        let arg = subArgs[i]
        switch arg {
        case "--dry-run":
            dryRun = true
        case "--no-checkpoint":
            noCheckpoint = true
        case "--upload":
            i += 1
            if i >= subArgs.count {
                fputs("Error: --upload requires a directory argument\n", stderr)
                printLaunchUsage()
            }
            uploadDirs.append(subArgs[i])
        case "--help", "-h":
            printLaunchUsage()
        default:
            if arg.hasPrefix("--") {
                fputs("Unknown option: \(arg)\n", stderr)
                printLaunchUsage()
            }
            positional.append(arg)
        }
        i += 1
    }

    if positional.isEmpty {
        printLaunchUsage()
    }

    let spriteName = positional[0]
    let planFile: String? = positional.count > 1 ? positional[1] : nil

    launchCommand(dryRun: dryRun, noCheckpoint: noCheckpoint,
                  uploadDirs: uploadDirs, spriteName: spriteName, planFile: planFile)

case "push":
    if subArgs.contains("--help") || subArgs.contains("-h") {
        printPushUsage()
    }
    if subArgs.count < 2 {
        printPushUsage()
    }
    let localPath = subArgs[0]
    let remotePath = subArgs[1]
    let spriteName: String? = subArgs.count > 2 ? subArgs[2] : nil

    pushCommand(localPath: localPath, remotePath: remotePath, spriteName: spriteName)

case "pull":
    if subArgs.contains("--help") || subArgs.contains("-h") {
        printPullUsage()
    }
    if subArgs.count < 2 {
        printPullUsage()
    }
    let remotePath = subArgs[0]
    let localPath = subArgs[1]
    let spriteName: String? = subArgs.count > 2 ? subArgs[2] : nil

    pullCommand(remotePath: remotePath, localPath: localPath, spriteName: spriteName)

case "watch":
    if subArgs.contains("--help") || subArgs.contains("-h") {
        printWatchUsage()
    }
    if subArgs.isEmpty {
        printWatchUsage()
    }
    let spriteName = subArgs[0]
    let taskId: String? = subArgs.count > 1 ? subArgs[1] : nil
    let pollInterval: Int? = subArgs.count > 2 ? Int(subArgs[2]) : nil

    watchCommand(spriteName: spriteName, taskId: taskId, pollInterval: pollInterval)

case "--help", "-h", "help":
    printUsage()

default:
    fputs("Unknown command: \(subcommand)\n", stderr)
    printUsage()
}
