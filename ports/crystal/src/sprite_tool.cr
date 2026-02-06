require "option_parser"

require "./config"
require "./sprite"
require "./launch"
require "./push"
require "./pull"
require "./watch"

module SpriteTool
  VERSION = "0.1.0"

  USAGE = <<-HELP
  Usage: sprite-tool <command> [options] [args]

  Commands:
    launch    Create and configure a sprite with coding agent, git, beads
    push      Push a local file or directory to a sprite
    pull      Pull a file or directory from a sprite
    watch     Watch a sprite's beads tracker task for progress

  Run 'sprite-tool <command> --help' for command-specific help.
  HELP

  def self.main
    if ARGV.empty? || ARGV.first == "--help" || ARGV.first == "-h"
      puts USAGE
      exit 0
    end

    command = ARGV.shift

    case command
    when "launch"
      run_launch
    when "push"
      run_push
    when "pull"
      run_pull
    when "watch"
      run_watch
    else
      STDERR.puts "Unknown command: #{command}"
      STDERR.puts USAGE
      exit 1
    end
  end

  private def self.run_launch
    config = Config.load

    # Collect remaining positional args after option parsing
    remaining = [] of String

    parser = OptionParser.new do |opts|
      opts.banner = <<-BANNER
      Usage: sprite-tool launch [options] <sprite-name> [plan-file]

      Create and configure a sprite with coding agent, git, beads.
      BANNER

      opts.on("--dry-run", "Show what would happen without executing") do
        config.dry_run = true
      end

      opts.on("--no-checkpoint", "Disable auto-checkpointing") do
        config.checkpointing = false
      end

      opts.on("--upload DIR", "Upload a local directory to /home/sprite/<dirname> (repeatable)") do |dir|
        config.upload_dirs << dir
      end

      opts.on("-h", "--help", "Show this help") do
        puts opts
        puts ""
        puts <<-ENV_HELP
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
          MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md
          AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md
        ENV_HELP
        exit 0
      end

      opts.unknown_args do |args|
        remaining = args
      end
    end

    parser.parse

    if remaining.empty?
      STDERR.puts "Error: sprite-name is required"
      STDERR.puts parser
      exit 1
    end

    sprite_name = remaining[0]
    plan_file = remaining[1]?

    Launch.run(config, sprite_name, plan_file)
  end

  private def self.run_push
    remaining = [] of String

    parser = OptionParser.new do |opts|
      opts.banner = <<-BANNER
      Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

      Push a local file or directory to a sprite.

      Examples:
        sprite-tool push ./file.txt /home/sprite/file.txt
        sprite-tool push ./mydir /home/sprite/mydir
        sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk
      BANNER

      opts.on("-h", "--help", "Show this help") do
        puts opts
        exit 0
      end

      opts.unknown_args do |args|
        remaining = args
      end
    end

    parser.parse

    if remaining.size < 2
      STDERR.puts "Error: local-path and remote-path are required"
      STDERR.puts parser
      exit 1
    end

    local_path = remaining[0]
    remote_path = remaining[1]
    sprite_name = remaining[2]?

    Push.run(local_path, remote_path, sprite_name)
  end

  private def self.run_pull
    remaining = [] of String

    parser = OptionParser.new do |opts|
      opts.banner = <<-BANNER
      Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

      Pull a file or directory from a sprite.

      Examples:
        sprite-tool pull /home/sprite/file.txt ./file.txt
        sprite-tool pull /home/sprite/mydir ./mydir
        sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk
      BANNER

      opts.on("-h", "--help", "Show this help") do
        puts opts
        exit 0
      end

      opts.unknown_args do |args|
        remaining = args
      end
    end

    parser.parse

    if remaining.size < 2
      STDERR.puts "Error: remote-path and local-path are required"
      STDERR.puts parser
      exit 1
    end

    remote_path = remaining[0]
    local_path = remaining[1]
    sprite_name = remaining[2]?

    Pull.run(remote_path, local_path, sprite_name)
  end

  private def self.run_watch
    remaining = [] of String

    parser = OptionParser.new do |opts|
      opts.banner = <<-BANNER
      Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

      Watch a sprite's beads tracker task for progress.

      Arguments:
        sprite-name     Name of the sprite to watch
        task-id         Beads task ID to track (default: auto-detect first open critical task)
        poll-interval   Seconds between polls (default: 30)

      Examples:
        sprite-tool watch ember-red-hawk
        sprite-tool watch ember-red-hawk CRM-1
        sprite-tool watch ember-red-hawk CRM-1 60
      BANNER

      opts.on("-h", "--help", "Show this help") do
        puts opts
        exit 0
      end

      opts.unknown_args do |args|
        remaining = args
      end
    end

    parser.parse

    if remaining.empty?
      STDERR.puts "Error: sprite-name is required"
      STDERR.puts parser
      exit 1
    end

    sprite_name = remaining[0]
    task_id = remaining[1]?
    poll_interval = remaining[2]? || "30"

    Watch.run(sprite_name, task_id, poll_interval)
  end
end

# Entry point
SpriteTool.main
