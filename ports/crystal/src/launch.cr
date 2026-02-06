module SpriteTool
  module Launch
    # Run the launch subcommand with the given config, sprite name, and optional plan file.
    def self.run(config : Config, sprite_name : String, plan_file : String?)
      sprite = Sprite.new(sprite_name, dry_run: config.dry_run)

      # 1. Check/install sprite CLI
      Sprite.ensure_cli(dry_run: config.dry_run)

      # 2. Auth sprite
      Sprite.auth(config.sprite_token, dry_run: config.dry_run)

      # 3. Create sprite (or use existing)
      if config.dry_run
        puts "  [dry-run] Would create or reuse sprite '#{sprite_name}'"
      elsif sprite.exists?
        puts "Sprite '#{sprite_name}' already exists, using it."
      else
        puts "Creating sprite: #{sprite_name}"
        sprite.create
      end

      # 4. Push .env to sprite
      if File.exists?(config.env_file) && File.file?(config.env_file)
        puts "Pushing #{config.env_file}..."
        sprite.push_file(config.env_file, "/home/sprite/.env")
      end

      # 5. Push plan file if provided
      if plan_file && !plan_file.empty? && File.exists?(plan_file) && File.file?(plan_file)
        puts "Pushing #{plan_file}..."
        sprite.push_file(plan_file, "/home/sprite/plan.md")
      end

      # 6. Upload directories if provided
      config.upload_dirs.each do |dir|
        if File.directory?(dir)
          dirname = File.basename(dir)
          puts "Uploading directory: #{dir} -> /home/sprite/#{dirname}"
          sprite.push_dir(dir, "/home/sprite/#{dirname}")
        else
          puts "WARNING: --upload dir '#{dir}' not found, skipping."
        end
      end

      # 7. Setup git + beads
      puts "Initializing git..."
      sprite.exec("cd /home/sprite && git init -b main 2>/dev/null || true")

      puts "Installing beads..."
      sprite.exec("curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash")

      # 8. Install and auth coding agent
      setup_agent(config, sprite)

      # 9. Launch agent with plan (or open console)
      puts ""
      puts "=========================================="
      puts "Sprite '#{sprite_name}' is ready!"
      model_info = config.model.empty? ? "" : " (model: #{config.model})"
      puts "Agent: #{config.agent}#{model_info}"
      puts "Checkpointing: every #{config.checkpoint_interval}s" if config.checkpointing
      puts "=========================================="

      if config.dry_run
        puts ""
        puts "[dry-run] Would launch #{config.agent} with plan. No changes were made."
        return
      end

      if plan_file && !plan_file.empty?
        # Start auto-checkpointing before agent runs
        checkpoint_channel = start_checkpointing(sprite, config) if config.checkpointing

        puts "Launching #{config.agent} with plan..."
        launch_agent(config, sprite)

        # Stop checkpointing and do final checkpoint
        stop_checkpointing(checkpoint_channel)
        puts "Creating final checkpoint..."
        if sprite.checkpoint_create
          puts "Final checkpoint saved."
        else
          puts "Final checkpoint failed (non-fatal)."
        end
      else
        puts "Opening console..."
        sprite.console
      end
    end

    # Set up the coding agent (claude or opencode) on the sprite.
    private def self.setup_agent(config : Config, sprite : Sprite)
      case config.agent
      when "claude"
        puts "Setting up claude..."
        sprite.exec("command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code")

        case config.claude_auth
        when "subscription"
          home = ENV["HOME"]? || ""
          creds_path = File.join(home, ".claude", ".credentials.json")
          if File.exists?(creds_path) && File.file?(creds_path)
            puts "Copying claude subscription credentials..."
            sprite.push_file(creds_path, "/home/sprite/.claude/.credentials.json")
            sprite.exec("chmod 600 ~/.claude/.credentials.json")
          else
            STDERR.puts "ERROR: ~/.claude/.credentials.json not found"
            STDERR.puts "Run 'claude' locally first to authenticate, then re-run this script."
            exit 1
          end
        when "apikey"
          if !config.anthropic_api_key.empty?
            puts "Setting ANTHROPIC_API_KEY in sprite..."
            escaped_key = config.anthropic_api_key
            sprite.exec(
              "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || " \
              "echo 'export ANTHROPIC_API_KEY=\"#{escaped_key}\"' >> ~/.bashrc"
            )
          else
            STDERR.puts "ERROR: No valid claude auth configured"
            STDERR.puts "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY"
            exit 1
          end
        else
          STDERR.puts "ERROR: No valid claude auth configured"
          STDERR.puts "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY"
          exit 1
        end

      when "opencode"
        puts "Setting up opencode..."
        sprite.exec("[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash")
        sprite.exec(
          "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || " \
          "echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc"
        )

      else
        STDERR.puts "ERROR: Unknown AGENT '#{config.agent}'. Use 'claude' or 'opencode'."
        exit 1
      end
    end

    # Launch the agent on the sprite with the plan.
    private def self.launch_agent(config : Config, sprite : Sprite)
      case config.agent
      when "claude"
        model_flag = config.model.empty? ? "" : "--model #{config.model} "
        sprite.exec_interactive(
          "cd /home/sprite && claude #{model_flag}-p 'read plan.md and complete the plan please'"
        )

      when "opencode"
        oc_model = config.model.empty? ? "opencode/big-pickle" : config.model
        sprite.exec_interactive(
          "set -a && source /home/sprite/.env 2>/dev/null && set +a && " \
          "cd /home/sprite && ~/.opencode/bin/opencode run -m #{oc_model} " \
          "'read plan.md and complete the plan please'"
        )
      end
    end

    # Start the background checkpointing fiber.
    # Returns a Channel used to signal stop.
    private def self.start_checkpointing(sprite : Sprite, config : Config) : Channel(Nil)?
      return nil unless config.checkpointing

      stop_channel = Channel(Nil).new
      interval = config.checkpoint_interval

      spawn do
        loop do
          # Use a select to either sleep for the interval or receive the stop signal
          select
          when stop_channel.receive
            break
          when timeout(interval.seconds)
            time_str = Time.local.to_s("%H:%M:%S")
            puts "[checkpoint] Creating checkpoint at #{time_str}..."
            if sprite.checkpoint_create
              puts "[checkpoint] Done."
            else
              puts "[checkpoint] Failed (non-fatal)."
            end
          end
        end
      end

      puts "Auto-checkpointing every #{interval}s"

      stop_channel
    end

    # Stop the background checkpointing fiber.
    private def self.stop_checkpointing(channel : Channel(Nil)?)
      return unless channel
      begin
        channel.send(nil)
      rescue Channel::ClosedError
        # Already stopped
      end
    end
  end
end
