# frozen_string_literal: true

require "optparse"

module SpriteTool
  module CLI
    USAGE = <<~HELP
      Usage: sprite-tool <command> [options] [args]

      Commands:
        launch    Create and configure a sprite with coding agent, git, beads
        push      Push a local file or directory to a sprite
        pull      Pull a file or directory from a sprite
        watch     Watch a sprite's beads tracker task for progress

      Run 'sprite-tool <command> --help' for command-specific help.
    HELP

    def self.run(argv = ARGV)
      if argv.empty? || argv.first == "--help" || argv.first == "-h"
        puts USAGE
        exit 0
      end

      command = argv.shift

      case command
      when "launch"
        run_launch(argv)
      when "push"
        run_push(argv)
      when "pull"
        run_pull(argv)
      when "watch"
        run_watch(argv)
      else
        $stderr.puts "Unknown command: #{command}"
        $stderr.puts USAGE
        exit 1
      end
    end

    def self.run_launch(argv)
      config = Config.load

      parser = OptionParser.new do |opts|
        opts.banner = <<~BANNER
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
          puts <<~ENV_HELP
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
      end

      remaining = parser.parse(argv)

      if remaining.empty?
        $stderr.puts "Error: sprite-name is required"
        $stderr.puts parser.help
        exit 1
      end

      sprite_name = remaining[0]
      plan_file = remaining[1]

      Launch.run(config, sprite_name, plan_file)
    end

    def self.run_push(argv)
      parser = OptionParser.new do |opts|
        opts.banner = <<~BANNER
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
      end

      remaining = parser.parse(argv)

      if remaining.length < 2
        $stderr.puts "Error: local-path and remote-path are required"
        $stderr.puts parser.help
        exit 1
      end

      local_path = remaining[0]
      remote_path = remaining[1]
      sprite_name = remaining[2]

      Push.run(local_path, remote_path, sprite_name)
    end

    def self.run_pull(argv)
      parser = OptionParser.new do |opts|
        opts.banner = <<~BANNER
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
      end

      remaining = parser.parse(argv)

      if remaining.length < 2
        $stderr.puts "Error: remote-path and local-path are required"
        $stderr.puts parser.help
        exit 1
      end

      remote_path = remaining[0]
      local_path = remaining[1]
      sprite_name = remaining[2]

      Pull.run(remote_path, local_path, sprite_name)
    end

    def self.run_watch(argv)
      parser = OptionParser.new do |opts|
        opts.banner = <<~BANNER
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
      end

      remaining = parser.parse(argv)

      if remaining.empty?
        $stderr.puts "Error: sprite-name is required"
        $stderr.puts parser.help
        exit 1
      end

      sprite_name = remaining[0]
      task_id = remaining[1]
      poll_interval = remaining[2] || "30"

      Watch.run(sprite_name, task_id, poll_interval)
    end

    private_class_method :run_launch, :run_push, :run_pull, :run_watch
  end
end
