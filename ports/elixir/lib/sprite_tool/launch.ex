defmodule SpriteTool.Launch do
  @moduledoc """
  sprite-tool launch: Create and configure a sprite with coding agent, git, beads.
  """

  alias SpriteTool.Config
  alias SpriteTool.Sprite

  @doc """
  Execute the launch subcommand.
  """
  @spec run([String.t()]) :: :ok
  def run(args) do
    # Load config from .env + environment
    cfg = Config.load()

    # Parse flags
    {cfg, positional} = parse_args(args, cfg)

    case positional do
      [] ->
        usage()
        System.halt(1)

      [sprite_name | rest] ->
        plan_file = List.first(rest, "")
        do_launch(cfg, sprite_name, plan_file)
    end
  end

  # ------------------------------------------------------------------
  # Argument parsing
  # ------------------------------------------------------------------

  defp parse_args(args, cfg) do
    parse_args(args, cfg, [])
  end

  defp parse_args([], cfg, positional) do
    {cfg, Enum.reverse(positional)}
  end

  defp parse_args(["--dry-run" | rest], cfg, positional) do
    parse_args(rest, %{cfg | dry_run: true}, positional)
  end

  defp parse_args(["--no-checkpoint" | rest], cfg, positional) do
    parse_args(rest, %{cfg | checkpointing: false}, positional)
  end

  defp parse_args(["--upload", dir | rest], cfg, positional) do
    parse_args(rest, %{cfg | upload_dirs: cfg.upload_dirs ++ [dir]}, positional)
  end

  defp parse_args(["--upload"], _cfg, _positional) do
    IO.puts(:stderr, "Error: --upload requires an argument")
    System.halt(1)
  end

  defp parse_args(["--help" | _], _cfg, _positional) do
    usage()
    System.halt(0)
  end

  defp parse_args(["-h" | _], _cfg, _positional) do
    usage()
    System.halt(0)
  end

  defp parse_args(["--" <> flag | _], _cfg, _positional) do
    IO.puts("Unknown option: --#{flag}")
    usage()
    System.halt(1)
  end

  defp parse_args([arg | rest], cfg, positional) do
    parse_args(rest, cfg, [arg | positional])
  end

  # ------------------------------------------------------------------
  # Main launch logic
  # ------------------------------------------------------------------

  defp do_launch(cfg, sprite_name, plan_file) do
    dry_run = cfg.dry_run

    # 1. Check/install sprite CLI
    check_sprite_cli(dry_run)

    # 2. Auth sprite (non-interactive if token provided)
    auth_sprite(cfg, dry_run)

    # 3. Create sprite (or use existing)
    create_sprite(sprite_name, dry_run)

    # 4. Push .env to sprite
    if File.regular?(cfg.env_file) do
      IO.puts("Pushing #{cfg.env_file}...")
      Sprite.push_file(sprite_name, cfg.env_file, "/home/sprite/.env", dry_run)
    end

    # 5. Push plan file if provided
    if plan_file != "" and File.regular?(plan_file) do
      IO.puts("Pushing #{plan_file}...")
      Sprite.push_file(sprite_name, plan_file, "/home/sprite/plan.md", dry_run)
    end

    # 6. Upload directories if provided
    Enum.each(cfg.upload_dirs, fn dir ->
      if File.dir?(dir) do
        dirname = Path.basename(dir)
        IO.puts("Uploading directory: #{dir} -> /home/sprite/#{dirname}")
        Sprite.push_dir(sprite_name, dir, "/home/sprite/#{dirname}", dry_run)
      else
        IO.puts("WARNING: --upload dir '#{dir}' not found, skipping.")
      end
    end)

    # 7. Setup git + beads
    IO.puts("Initializing git...")
    Sprite.sx(sprite_name, "cd /home/sprite && git init -b main 2>/dev/null || true", dry_run)

    IO.puts("Installing beads...")

    Sprite.sx(
      sprite_name,
      "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash",
      dry_run
    )

    # 8. Install and auth coding agent
    setup_agent(cfg, sprite_name, dry_run)

    # 9. Launch agent with plan (or open console)
    announce(cfg, sprite_name)

    if dry_run do
      IO.puts("")
      IO.puts("[dry-run] Would launch #{cfg.agent} with plan. No changes were made.")
      :ok
    else
      if plan_file != "" do
        launch_with_plan(cfg, sprite_name)
      else
        IO.puts("Opening console...")
        System.cmd("sprite", ["console", "-s", sprite_name], into: IO.stream(:stdio, :line))
        :ok
      end
    end
  end

  # ------------------------------------------------------------------
  # Step helpers
  # ------------------------------------------------------------------

  defp check_sprite_cli(dry_run) do
    if Sprite.find_sprite_cli() == nil do
      if dry_run do
        IO.puts("  [dry-run] Would install sprite CLI")
      else
        IO.puts("Installing sprite CLI...")
        System.cmd("bash", ["-c", "curl -fsSL https://sprites.dev/install.sh | sh"])
        # Add common install location to PATH
        local_bin = Path.join(System.get_env("HOME") || "~", ".local/bin")
        current_path = System.get_env("PATH") || ""
        System.put_env("PATH", "#{local_bin}:#{current_path}")
      end
    end
  end

  defp auth_sprite(cfg, dry_run) do
    if cfg.sprite_token != "" do
      IO.puts("Authenticating sprite with token...")

      unless dry_run do
        System.cmd("sprite", ["auth", "setup", "--token", cfg.sprite_token])
      end
    else
      IO.puts("No SPRITE_TOKEN set. Running interactive login...")

      unless dry_run do
        System.cmd("sprite", ["login"], into: IO.stream(:stdio, :line))
      end
    end
  end

  defp create_sprite(sprite_name, dry_run) do
    if dry_run do
      IO.puts("  [dry-run] Would create or reuse sprite '#{sprite_name}'")
    else
      if Sprite.sprite_exists?(sprite_name) do
        IO.puts("Sprite '#{sprite_name}' already exists, using it.")
      else
        IO.puts("Creating sprite: #{sprite_name}")
        System.cmd("sprite", ["create", "-skip-console", sprite_name])
      end
    end
  end

  defp setup_agent(cfg, sprite_name, dry_run) do
    case cfg.agent do
      "claude" ->
        setup_claude(cfg, sprite_name, dry_run)

      "opencode" ->
        setup_opencode(sprite_name, dry_run)

      other ->
        IO.puts("ERROR: Unknown AGENT '#{other}'. Use 'claude' or 'opencode'.")
        System.halt(1)
    end
  end

  defp setup_claude(cfg, sprite_name, dry_run) do
    IO.puts("Setting up claude...")

    Sprite.sx(
      sprite_name,
      "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code",
      dry_run
    )

    case cfg.claude_auth do
      "subscription" ->
        home = System.get_env("HOME") || "~"
        creds_path = Path.join([home, ".claude", ".credentials.json"])

        if File.regular?(creds_path) do
          IO.puts("Copying claude subscription credentials...")
          Sprite.push_file(sprite_name, creds_path, "/home/sprite/.claude/.credentials.json", dry_run)
          Sprite.sx(sprite_name, "chmod 600 ~/.claude/.credentials.json", dry_run)
        else
          IO.puts("ERROR: ~/.claude/.credentials.json not found")
          IO.puts("Run 'claude' locally first to authenticate, then re-run this script.")
          System.halt(1)
        end

      "apikey" when cfg.anthropic_api_key != "" ->
        IO.puts("Setting ANTHROPIC_API_KEY in sprite...")

        Sprite.sx(
          sprite_name,
          "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"#{cfg.anthropic_api_key}\"' >> ~/.bashrc",
          dry_run
        )

      _ ->
        IO.puts("ERROR: No valid claude auth configured")
        IO.puts("Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY")
        System.halt(1)
    end
  end

  defp setup_opencode(sprite_name, dry_run) do
    IO.puts("Setting up opencode...")

    Sprite.sx(
      sprite_name,
      "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash",
      dry_run
    )

    Sprite.sx(
      sprite_name,
      "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc",
      dry_run
    )
  end

  defp announce(cfg, sprite_name) do
    IO.puts("")
    IO.puts("==========================================")
    IO.puts("Sprite '#{sprite_name}' is ready!")

    model_note = if cfg.model != "", do: " (model: #{cfg.model})", else: ""
    IO.puts("Agent: #{cfg.agent}#{model_note}")

    if cfg.checkpointing do
      IO.puts("Checkpointing: every #{cfg.checkpoint_interval}s")
    end

    IO.puts("==========================================")
  end

  # ------------------------------------------------------------------
  # Launch with plan + checkpointing
  # ------------------------------------------------------------------

  defp launch_with_plan(cfg, sprite_name) do
    # Start auto-checkpointing before agent runs
    checkpoint_pid =
      if cfg.checkpointing do
        start_checkpointing(sprite_name, cfg.checkpoint_interval)
      else
        nil
      end

    # Trap exits so we can clean up the checkpoint process
    Process.flag(:trap_exit, true)

    IO.puts("Launching #{cfg.agent} with plan...")

    case cfg.agent do
      "claude" ->
        model_flag = if cfg.model != "", do: "--model #{cfg.model} ", else: ""

        Sprite.sx_passthrough(
          sprite_name,
          "cd /home/sprite && claude #{model_flag}-p 'read plan.md and complete the plan please'"
        )

      "opencode" ->
        oc_model = if cfg.model != "", do: cfg.model, else: "opencode/big-pickle"

        Sprite.sx_passthrough(
          sprite_name,
          "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m #{oc_model} 'read plan.md and complete the plan please'"
        )
    end

    # Final checkpoint after agent completes
    stop_checkpointing(checkpoint_pid)

    IO.puts("Creating final checkpoint...")

    case System.cmd("sprite", ["checkpoint", "create", "-s", sprite_name],
           stderr_to_stdout: true
         ) do
      {_, 0} -> IO.puts("Final checkpoint saved.")
      {_, _} -> IO.puts("Final checkpoint failed (non-fatal).")
    end

    :ok
  end

  # ------------------------------------------------------------------
  # Checkpoint loop (background process)
  # ------------------------------------------------------------------

  defp start_checkpointing(sprite_name, interval) do
    parent = self()

    pid =
      spawn_link(fn ->
        checkpoint_loop(sprite_name, interval, parent)
      end)

    IO.puts("Auto-checkpointing every #{interval}s (pid: #{inspect(pid)})")
    pid
  end

  defp checkpoint_loop(sprite_name, interval, parent) do
    Process.sleep(interval * 1000)

    now = Calendar.strftime(DateTime.utc_now(), "%H:%M:%S")
    IO.puts("[checkpoint] Creating checkpoint at #{now}...")

    case System.cmd("sprite", ["checkpoint", "create", "-s", sprite_name],
           stderr_to_stdout: true
         ) do
      {_, 0} -> IO.puts("[checkpoint] Done.")
      {_, _} -> IO.puts("[checkpoint] Failed (non-fatal).")
    end

    checkpoint_loop(sprite_name, interval, parent)
  end

  defp stop_checkpointing(nil), do: :ok

  defp stop_checkpointing(pid) when is_pid(pid) do
    if Process.alive?(pid) do
      Process.exit(pid, :shutdown)

      receive do
        {:EXIT, ^pid, _reason} -> :ok
      after
        5_000 -> :ok
      end
    end
  end

  # ------------------------------------------------------------------
  # Usage
  # ------------------------------------------------------------------

  defp usage do
    IO.puts("""
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
      MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md
      AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md\
    """)
  end
end
