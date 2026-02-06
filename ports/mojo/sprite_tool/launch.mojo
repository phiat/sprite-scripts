"""sprite-tool launch: Create and configure a sprite with coding agent, git, beads."""

from python import Python, PythonObject

from .config import Config, load as config_load
from .sprite import (
    find_sprite_cli,
    push_dir,
    push_file,
    sprite_exists,
    sx,
    sx_passthrough,
)


def _usage() raises:
    print(
        "Usage: sprite-tool launch [options] <sprite-name> [plan-file]\n"
        "\n"
        "Options:\n"
        "  --dry-run              Show what would happen without executing\n"
        "  --no-checkpoint        Disable auto-checkpointing\n"
        "  --upload <dir>         Upload a local directory to /home/sprite/<dirname>\n"
        "                         (repeatable: --upload ./data --upload ./tests)\n"
        "\n"
        "Environment variables:\n"
        "  ENV_FILE               Path to .env file (default: ./.env)\n"
        "  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)\n"
        '  AGENT                  "opencode" (default) or "claude"\n'
        '  CLAUDE_AUTH            "subscription" (default) or "apikey"\n'
        "  MODEL                  Model override (see below)\n"
        "  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)\n"
        "\n"
        "Model examples:\n"
        "  OpenCode: MODEL=opencode/big-pickle  (free, default)\n"
        "            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)\n"
        "            MODEL=openai/gpt-4o\n"
        "            MODEL=google/gemini-2.5-pro\n"
        "  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku\n"
        "\n"
        "Examples:\n"
        "  sprite-tool launch my-project plan.md\n"
        "  sprite-tool launch --upload ./data my-project plan.md\n"
        "  sprite-tool launch --upload ./data --upload ./tests my-project plan.md\n"
        "  sprite-tool launch --dry-run my-project plan.md\n"
        "  MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md\n"
        "  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md"
    )
    var sys_mod = Python.import_module("sys")
    _ = sys_mod.exit(1)


def _start_checkpoint_thread(sprite_name: String, interval: Int) raises -> PythonObject:
    """Start a background checkpoint thread. Returns (thread, stop_event) as Python tuple."""
    var threading = Python.import_module("threading")
    var time_mod = Python.import_module("time")
    var subprocess = Python.import_module("subprocess")

    var stop_event = threading.Event()

    # Build the thread target as a Python function via exec since Mojo closures
    # cannot directly serve as Python thread targets.
    var py_builtins = Python.import_module("builtins")

    var code = (
        "import subprocess, time, threading\n"
        "def _checkpoint_loop(stop_event, sprite_name, interval):\n"
        "    while not stop_event.is_set():\n"
        "        if stop_event.wait(timeout=interval):\n"
        "            break\n"
        "        now = time.strftime('%H:%M:%S')\n"
        "        print(f'[checkpoint] Creating checkpoint at {now}...')\n"
        "        try:\n"
        "            result = subprocess.run(\n"
        "                ['sprite', 'checkpoint', 'create', '-s', sprite_name],\n"
        "                capture_output=True, text=True,\n"
        "            )\n"
        "            if result.returncode == 0:\n"
        "                print('[checkpoint] Done.')\n"
        "            else:\n"
        "                print('[checkpoint] Failed (non-fatal).')\n"
        "        except Exception:\n"
        "            print('[checkpoint] Failed (non-fatal).')\n"
        "def make_thread(target, stop_event, sprite_name, interval):\n"
        "    t = threading.Thread(target=target, args=(stop_event, sprite_name, interval), daemon=True)\n"
        "    return t\n"
    )
    var ns = Python.dict()
    _ = py_builtins.exec(code, ns)
    var target_fn = ns["_checkpoint_loop"]
    var make_thread = ns["make_thread"]

    var thread = make_thread(target_fn, stop_event, sprite_name, interval)
    thread.start()
    print("Auto-checkpointing every " + str(interval) + "s (thread)")

    # Return stop_event and thread as a Python tuple
    var result = Python.list()
    result.append(stop_event)
    result.append(thread)
    return result


def _stop_checkpoint_thread(checkpoint_info: PythonObject) raises:
    """Stop the background checkpoint thread."""
    if checkpoint_info is Python.none():
        return
    var stop_event = checkpoint_info[0]
    var thread = checkpoint_info[1]
    stop_event.set()
    if thread.is_alive():
        thread.join(timeout=5)


def run(args: PythonObject) raises:
    """Execute the launch subcommand."""
    var sys_mod = Python.import_module("sys")
    var os = Python.import_module("os")
    var subprocess = Python.import_module("subprocess")
    var shutil = Python.import_module("shutil")

    # ----------------------------------------------------------------
    # Load config from .env + environment
    # ----------------------------------------------------------------
    var cfg = config_load()

    # ----------------------------------------------------------------
    # Parse flags
    # ----------------------------------------------------------------
    var positional = Python.list()
    var i: Int = 0
    var num_args = int(len(args))
    while i < num_args:
        var arg = str(args[i])
        if arg == "--dry-run":
            cfg.dry_run = True
        elif arg == "--no-checkpoint":
            cfg.checkpointing = False
        elif arg == "--upload":
            i += 1
            if i >= num_args:
                print("Error: --upload requires an argument")
                _ = sys_mod.exit(1)
            cfg.upload_dirs.append(args[i])
        elif arg == "--help" or arg == "-h":
            _usage()
        elif arg.startswith("--"):
            print("Unknown option: " + arg)
            _usage()
        else:
            positional.append(arg)
        i += 1

    if int(len(positional)) < 1:
        _usage()

    var sprite_name = str(positional[0])
    var plan_file: String = ""
    if int(len(positional)) > 1:
        plan_file = str(positional[1])

    var dry_run = cfg.dry_run

    # ----------------------------------------------------------------
    # 1. Check/install sprite CLI
    # ----------------------------------------------------------------
    if shutil.which("sprite") is Python.none():
        if dry_run:
            print("  [dry-run] Would install sprite CLI")
        else:
            print("Installing sprite CLI...")
            var install_args = Python.list()
            install_args.append("bash")
            install_args.append("-c")
            install_args.append("curl -fsSL https://sprites.dev/install.sh | sh")
            _ = subprocess.run(install_args, check=True)
            # Add common install location to PATH
            var home = str(os.path.expanduser("~"))
            var local_bin = home + "/.local/bin"
            os.environ["PATH"] = local_bin + ":" + str(os.environ.get("PATH", ""))

    # ----------------------------------------------------------------
    # 2. Auth sprite (non-interactive if token provided)
    # ----------------------------------------------------------------
    if len(cfg.sprite_token) > 0:
        print("Authenticating sprite with token...")
        if not dry_run:
            var auth_args = Python.list()
            auth_args.append("sprite")
            auth_args.append("auth")
            auth_args.append("setup")
            auth_args.append("--token")
            auth_args.append(cfg.sprite_token)
            _ = subprocess.run(auth_args, check=True)
    else:
        print("No SPRITE_TOKEN set. Running interactive login...")
        if not dry_run:
            var login_args = Python.list()
            login_args.append("sprite")
            login_args.append("login")
            _ = subprocess.run(login_args, check=True)

    # ----------------------------------------------------------------
    # 3. Create sprite (or use existing)
    # ----------------------------------------------------------------
    if dry_run:
        print("  [dry-run] Would create or reuse sprite '" + sprite_name + "'")
    elif sprite_exists(sprite_name):
        print("Sprite '" + sprite_name + "' already exists, using it.")
    else:
        print("Creating sprite: " + sprite_name)
        var create_args = Python.list()
        create_args.append("sprite")
        create_args.append("create")
        create_args.append("-skip-console")
        create_args.append(sprite_name)
        _ = subprocess.run(create_args, check=True)

    # ----------------------------------------------------------------
    # 4. Push .env to sprite
    # ----------------------------------------------------------------
    if os.path.isfile(cfg.env_file):
        print("Pushing " + cfg.env_file + "...")
        push_file(sprite_name, cfg.env_file, "/home/sprite/.env", dry_run)

    # ----------------------------------------------------------------
    # 5. Push plan file if provided
    # ----------------------------------------------------------------
    if len(plan_file) > 0 and bool(os.path.isfile(plan_file)):
        print("Pushing " + plan_file + "...")
        push_file(sprite_name, plan_file, "/home/sprite/plan.md", dry_run)

    # ----------------------------------------------------------------
    # 6. Upload directories if provided
    # ----------------------------------------------------------------
    var num_uploads = int(len(cfg.upload_dirs))
    for idx in range(num_uploads):
        var upload_dir = str(cfg.upload_dirs[idx])
        if bool(os.path.isdir(upload_dir)):
            var dirname = str(os.path.basename(upload_dir))
            print("Uploading directory: " + upload_dir + " -> /home/sprite/" + dirname)
            push_dir(sprite_name, upload_dir, "/home/sprite/" + dirname, dry_run)
        else:
            print("WARNING: --upload dir '" + upload_dir + "' not found, skipping.")

    # ----------------------------------------------------------------
    # 7. Setup git + beads
    # ----------------------------------------------------------------
    print("Initializing git...")
    _ = sx(sprite_name, "cd /home/sprite && git init -b main 2>/dev/null || true", dry_run)

    print("Installing beads...")
    _ = sx(
        sprite_name,
        "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash",
        dry_run,
    )

    # ----------------------------------------------------------------
    # 8. Install and auth coding agent
    # ----------------------------------------------------------------
    if cfg.agent == "claude":
        print("Setting up claude...")
        _ = sx(
            sprite_name,
            "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code",
            dry_run,
        )

        if cfg.claude_auth == "subscription":
            var home = str(os.path.expanduser("~"))
            var creds_path = home + "/.claude/.credentials.json"
            if bool(os.path.isfile(creds_path)):
                print("Copying claude subscription credentials...")
                push_file(sprite_name, creds_path, "/home/sprite/.claude/.credentials.json", dry_run)
                _ = sx(sprite_name, "chmod 600 ~/.claude/.credentials.json", dry_run)
            else:
                print("ERROR: ~/.claude/.credentials.json not found")
                print("Run 'claude' locally first to authenticate, then re-run this script.")
                _ = sys_mod.exit(1)

        elif cfg.claude_auth == "apikey" and len(cfg.anthropic_api_key) > 0:
            print("Setting ANTHROPIC_API_KEY in sprite...")
            _ = sx(
                sprite_name,
                "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\""
                + cfg.anthropic_api_key
                + "\"' >> ~/.bashrc",
                dry_run,
            )
        else:
            print("ERROR: No valid claude auth configured")
            print("Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY")
            _ = sys_mod.exit(1)

    elif cfg.agent == "opencode":
        print("Setting up opencode...")
        _ = sx(
            sprite_name,
            "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash",
            dry_run,
        )
        _ = sx(
            sprite_name,
            "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || "
            + "echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc",
            dry_run,
        )
    else:
        print("ERROR: Unknown AGENT '" + cfg.agent + "'. Use 'claude' or 'opencode'.")
        _ = sys_mod.exit(1)

    # ----------------------------------------------------------------
    # 9. Launch agent with plan (or open console)
    # ----------------------------------------------------------------
    print("")
    print("==========================================")
    print("Sprite '" + sprite_name + "' is ready!")
    var model_note: String = ""
    if len(cfg.model) > 0:
        model_note = " (model: " + cfg.model + ")"
    print("Agent: " + cfg.agent + model_note)
    if cfg.checkpointing:
        print("Checkpointing: every " + str(cfg.checkpoint_interval) + "s")
    print("==========================================")

    if dry_run:
        print("")
        print("[dry-run] Would launch " + cfg.agent + " with plan. No changes were made.")
        return

    var checkpoint_info: PythonObject = Python.none()

    # Register atexit cleanup via Python
    var atexit = Python.import_module("atexit")
    var builtins = Python.import_module("builtins")

    if len(plan_file) > 0:
        # Start auto-checkpointing before agent runs
        if cfg.checkpointing:
            checkpoint_info = _start_checkpoint_thread(sprite_name, cfg.checkpoint_interval)

        print("Launching " + cfg.agent + " with plan...")

        if cfg.agent == "claude":
            var model_flag: String = ""
            if len(cfg.model) > 0:
                model_flag = "--model " + cfg.model + " "
            _ = sx_passthrough(
                sprite_name,
                "cd /home/sprite && claude " + model_flag + "-p 'read plan.md and complete the plan please'",
                dry_run,
            )

        elif cfg.agent == "opencode":
            var oc_model = cfg.model
            if len(oc_model) == 0:
                oc_model = "opencode/big-pickle"
            _ = sx_passthrough(
                sprite_name,
                "set -a && source /home/sprite/.env 2>/dev/null && set +a && "
                + "cd /home/sprite && ~/.opencode/bin/opencode run -m "
                + oc_model
                + " 'read plan.md and complete the plan please'",
                dry_run,
            )

        # Final checkpoint after agent completes
        _stop_checkpoint_thread(checkpoint_info)
        print("Creating final checkpoint...")
        var final_args = Python.list()
        final_args.append("sprite")
        final_args.append("checkpoint")
        final_args.append("create")
        final_args.append("-s")
        final_args.append(sprite_name)
        var result = subprocess.run(final_args, capture_output=True, text=True)
        if int(result.returncode) == 0:
            print("Final checkpoint saved.")
        else:
            print("Final checkpoint failed (non-fatal).")
    else:
        print("Opening console...")
        var console_args = Python.list()
        console_args.append("sprite")
        console_args.append("console")
        console_args.append("-s")
        console_args.append(sprite_name)
        _ = subprocess.run(console_args)
