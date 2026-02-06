"""sprite-tool launch: Create and configure a sprite with coding agent, git, beads."""

import atexit
import os
import shutil
import signal
import subprocess
import sys
import threading
import time
from pathlib import Path

from . import config as config_mod
from . import sprite


def _usage():
    print("""\
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
  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md""")
    sys.exit(1)


class CheckpointLoop:
    """Background checkpoint loop using a daemon thread."""

    def __init__(self, sprite_name: str, interval: int):
        self._sprite_name = sprite_name
        self._interval = interval
        self._stop_event = threading.Event()
        self._thread = None

    def start(self):
        """Start the background checkpoint loop."""
        self._thread = threading.Thread(target=self._run, daemon=True)
        self._thread.start()
        print(f"Auto-checkpointing every {self._interval}s (thread)")

    def _run(self):
        while not self._stop_event.is_set():
            # Wait for the interval, but wake early if stop is called
            if self._stop_event.wait(timeout=self._interval):
                break
            now = time.strftime("%H:%M:%S")
            print(f"[checkpoint] Creating checkpoint at {now}...")
            try:
                result = subprocess.run(
                    ["sprite", "checkpoint", "create", "-s", self._sprite_name],
                    capture_output=True,
                    text=True,
                )
                if result.returncode == 0:
                    print("[checkpoint] Done.")
                else:
                    print("[checkpoint] Failed (non-fatal).")
            except Exception:
                print("[checkpoint] Failed (non-fatal).")

    def stop(self):
        """Stop the background checkpoint loop and wait for thread exit."""
        self._stop_event.set()
        if self._thread is not None and self._thread.is_alive():
            self._thread.join(timeout=5)


def run(args):
    """Execute the launch subcommand."""
    # ----------------------------------------------------------------
    # Load config from .env + environment
    # ----------------------------------------------------------------
    cfg = config_mod.load()

    # ----------------------------------------------------------------
    # Parse flags
    # ----------------------------------------------------------------
    positional = []
    i = 0
    while i < len(args):
        arg = args[i]
        if arg == "--dry-run":
            cfg.dry_run = True
        elif arg == "--no-checkpoint":
            cfg.checkpointing = False
        elif arg == "--upload":
            i += 1
            if i >= len(args):
                print("Error: --upload requires an argument")
                sys.exit(1)
            cfg.upload_dirs.append(args[i])
        elif arg in ("--help", "-h"):
            _usage()
        elif arg.startswith("--"):
            print(f"Unknown option: {arg}")
            _usage()
        else:
            positional.append(arg)
        i += 1

    if len(positional) < 1:
        _usage()

    sprite_name = positional[0]
    plan_file = positional[1] if len(positional) > 1 else ""

    dry_run = cfg.dry_run

    # Shortcuts
    def _sx(cmd):
        return sprite.sx(sprite_name, cmd, dry_run=dry_run)

    def _sx_pass(cmd):
        return sprite.sx_passthrough(sprite_name, cmd, dry_run=dry_run)

    def _push_file(src, dest):
        return sprite.push_file(sprite_name, src, dest, dry_run=dry_run)

    def _push_dir(src, dest):
        return sprite.push_dir(sprite_name, src, dest, dry_run=dry_run)

    # ----------------------------------------------------------------
    # 1. Check/install sprite CLI
    # ----------------------------------------------------------------
    if not shutil.which("sprite"):
        if dry_run:
            print("  [dry-run] Would install sprite CLI")
        else:
            print("Installing sprite CLI...")
            subprocess.run(
                ["bash", "-c", "curl -fsSL https://sprites.dev/install.sh | sh"],
                check=True,
            )
            # Add common install location to PATH
            local_bin = os.path.join(os.path.expanduser("~"), ".local", "bin")
            os.environ["PATH"] = local_bin + os.pathsep + os.environ.get("PATH", "")

    # ----------------------------------------------------------------
    # 2. Auth sprite (non-interactive if token provided)
    # ----------------------------------------------------------------
    if cfg.sprite_token:
        print("Authenticating sprite with token...")
        if not dry_run:
            subprocess.run(
                ["sprite", "auth", "setup", "--token", cfg.sprite_token],
                check=True,
            )
    else:
        print("No SPRITE_TOKEN set. Running interactive login...")
        if not dry_run:
            subprocess.run(["sprite", "login"], check=True)

    # ----------------------------------------------------------------
    # 3. Create sprite (or use existing)
    # ----------------------------------------------------------------
    if dry_run:
        print(f"  [dry-run] Would create or reuse sprite '{sprite_name}'")
    elif sprite.sprite_exists(sprite_name):
        print(f"Sprite '{sprite_name}' already exists, using it.")
    else:
        print(f"Creating sprite: {sprite_name}")
        subprocess.run(
            ["sprite", "create", "-skip-console", sprite_name],
            check=True,
        )

    # ----------------------------------------------------------------
    # 4. Push .env to sprite
    # ----------------------------------------------------------------
    if Path(cfg.env_file).is_file():
        print(f"Pushing {cfg.env_file}...")
        _push_file(cfg.env_file, "/home/sprite/.env")

    # ----------------------------------------------------------------
    # 5. Push plan file if provided
    # ----------------------------------------------------------------
    if plan_file and Path(plan_file).is_file():
        print(f"Pushing {plan_file}...")
        _push_file(plan_file, "/home/sprite/plan.md")

    # ----------------------------------------------------------------
    # 6. Upload directories if provided
    # ----------------------------------------------------------------
    for upload_dir in cfg.upload_dirs:
        if Path(upload_dir).is_dir():
            dirname = Path(upload_dir).name
            print(f"Uploading directory: {upload_dir} -> /home/sprite/{dirname}")
            _push_dir(upload_dir, f"/home/sprite/{dirname}")
        else:
            print(f"WARNING: --upload dir '{upload_dir}' not found, skipping.")

    # ----------------------------------------------------------------
    # 7. Setup git + beads
    # ----------------------------------------------------------------
    print("Initializing git...")
    _sx("cd /home/sprite && git init -b main 2>/dev/null || true")

    print("Installing beads...")
    _sx(
        "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash"
    )

    # ----------------------------------------------------------------
    # 8. Install and auth coding agent
    # ----------------------------------------------------------------
    if cfg.agent == "claude":
        print("Setting up claude...")
        _sx(
            "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code"
        )

        if cfg.claude_auth == "subscription":
            creds_path = os.path.join(
                os.path.expanduser("~"), ".claude", ".credentials.json"
            )
            if Path(creds_path).is_file():
                print("Copying claude subscription credentials...")
                _push_file(creds_path, "/home/sprite/.claude/.credentials.json")
                _sx("chmod 600 ~/.claude/.credentials.json")
            else:
                print("ERROR: ~/.claude/.credentials.json not found")
                print(
                    "Run 'claude' locally first to authenticate, then re-run this script."
                )
                sys.exit(1)

        elif cfg.claude_auth == "apikey" and cfg.anthropic_api_key:
            print("Setting ANTHROPIC_API_KEY in sprite...")
            _sx(
                f"grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || "
                f"echo 'export ANTHROPIC_API_KEY=\"{cfg.anthropic_api_key}\"' >> ~/.bashrc"
            )
        else:
            print("ERROR: No valid claude auth configured")
            print(
                "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY"
            )
            sys.exit(1)

    elif cfg.agent == "opencode":
        print("Setting up opencode...")
        _sx(
            "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash"
        )
        _sx(
            "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || "
            "echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc"
        )
    else:
        print(f"ERROR: Unknown AGENT '{cfg.agent}'. Use 'claude' or 'opencode'.")
        sys.exit(1)

    # ----------------------------------------------------------------
    # 9. Launch agent with plan (or open console)
    # ----------------------------------------------------------------
    print()
    print("==========================================")
    print(f"Sprite '{sprite_name}' is ready!")
    model_note = f" (model: {cfg.model})" if cfg.model else ""
    print(f"Agent: {cfg.agent}{model_note}")
    if cfg.checkpointing:
        print(f"Checkpointing: every {cfg.checkpoint_interval}s")
    print("==========================================")

    if dry_run:
        print()
        print(f"[dry-run] Would launch {cfg.agent} with plan. No changes were made.")
        return

    # Setup checkpoint cleanup
    checkpoint_loop = None

    def _cleanup():
        if checkpoint_loop is not None:
            checkpoint_loop.stop()

    atexit.register(_cleanup)

    # Handle SIGTERM gracefully
    original_sigterm = signal.getsignal(signal.SIGTERM)

    def _sigterm_handler(signum, frame):
        _cleanup()
        # Restore original handler and re-raise
        signal.signal(signal.SIGTERM, original_sigterm)
        os.kill(os.getpid(), signal.SIGTERM)

    signal.signal(signal.SIGTERM, _sigterm_handler)

    if plan_file:
        # Start auto-checkpointing before agent runs
        if cfg.checkpointing:
            checkpoint_loop = CheckpointLoop(sprite_name, cfg.checkpoint_interval)
            checkpoint_loop.start()

        print(f"Launching {cfg.agent} with plan...")

        if cfg.agent == "claude":
            model_flag = ""
            if cfg.model:
                model_flag = f"--model {cfg.model} "
            _sx_pass(
                f"cd /home/sprite && claude {model_flag}-p "
                f"'read plan.md and complete the plan please'"
            )

        elif cfg.agent == "opencode":
            oc_model = cfg.model if cfg.model else "opencode/big-pickle"
            _sx_pass(
                f"set -a && source /home/sprite/.env 2>/dev/null && set +a && "
                f"cd /home/sprite && ~/.opencode/bin/opencode run -m {oc_model} "
                f"'read plan.md and complete the plan please'"
            )

        # Final checkpoint after agent completes
        if checkpoint_loop is not None:
            checkpoint_loop.stop()
        print("Creating final checkpoint...")
        result = subprocess.run(
            ["sprite", "checkpoint", "create", "-s", sprite_name],
            capture_output=True,
            text=True,
        )
        if result.returncode == 0:
            print("Final checkpoint saved.")
        else:
            print("Final checkpoint failed (non-fatal).")
    else:
        print("Opening console...")
        subprocess.run(["sprite", "console", "-s", sprite_name])
