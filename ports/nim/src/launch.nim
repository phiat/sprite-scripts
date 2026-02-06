## launch.nim - sprite-tool launch subcommand
## Creates and configures a sprite with a coding agent, git, beads, and optional checkpointing.

import os, strutils, times, locks
import config, sprite

const launchUsage* = """
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

# Checkpoint thread state
var
  checkpointLock: Lock
  checkpointRunning: bool = false
  checkpointSprite: string = ""
  checkpointIntervalSecs: int = 300

proc checkpointLoop() {.thread.} =
  ## Background thread that creates periodic checkpoints.
  {.cast(gcsafe).}:
    while true:
      sleep(checkpointIntervalSecs * 1000)
      acquire(checkpointLock)
      let running = checkpointRunning
      let spriteName = checkpointSprite
      release(checkpointLock)
      if not running:
        break
      let timeStr = now().format("HH:mm:ss")
      echo "[checkpoint] Creating checkpoint at " & timeStr & "..."
      let (_, exitCode) = spriteCmdCapture("checkpoint", "create", "-s", spriteName)
      if exitCode == 0:
        echo "[checkpoint] Done."
      else:
        echo "[checkpoint] Failed (non-fatal)."

var checkpointThread: Thread[void]

proc startCheckpointing(spriteName: string, interval: int) =
  initLock(checkpointLock)
  acquire(checkpointLock)
  checkpointRunning = true
  checkpointSprite = spriteName
  checkpointIntervalSecs = interval
  release(checkpointLock)
  createThread(checkpointThread, checkpointLoop)
  echo "Auto-checkpointing every " & $interval & "s"

proc stopCheckpointing() =
  acquire(checkpointLock)
  checkpointRunning = false
  release(checkpointLock)
  joinThread(checkpointThread)
  deinitLock(checkpointLock)

proc runLaunch*(args: seq[string]) =
  ## Parse launch-specific args and execute the launch sequence.
  var
    dryRun = false
    checkpointing = true
    uploadDirs: seq[string] = @[]
    positional: seq[string] = @[]

  var i = 0
  while i < args.len:
    let arg = args[i]
    case arg
    of "--dry-run":
      dryRun = true
    of "--no-checkpoint":
      checkpointing = false
    of "--upload":
      inc i
      if i >= args.len:
        echo "ERROR: --upload requires a directory argument"
        quit(1)
      uploadDirs.add(args[i])
    of "--help", "-h":
      echo launchUsage
      quit(0)
    else:
      if arg.startsWith("--"):
        echo "Unknown option: " & arg
        echo launchUsage
        quit(1)
      positional.add(arg)
    inc i

  if positional.len < 1:
    echo launchUsage
    quit(1)

  let spriteName = positional[0]
  let planFile = if positional.len > 1: positional[1] else: ""

  # Load config from environment + .env
  let cfg = loadConfig()

  # ============================================================
  # 1. Check/install sprite CLI
  # ============================================================
  ensureSpriteCli(dryRun)

  # ============================================================
  # 2. Auth sprite
  # ============================================================
  authSprite(cfg.spriteToken, dryRun)

  # ============================================================
  # 3. Create sprite (or use existing)
  # ============================================================
  if dryRun:
    echo "  [dry-run] Would create or reuse sprite '" & spriteName & "'"
  elif spriteExists(spriteName):
    echo "Sprite '" & spriteName & "' already exists, using it."
  else:
    echo "Creating sprite: " & spriteName
    let rc = spriteCmd("create", "-skip-console", spriteName)
    if rc != 0:
      echo "ERROR: Failed to create sprite"
      quit(1)

  # ============================================================
  # 4. Push .env to sprite
  # ============================================================
  if fileExists(cfg.envFile):
    echo "Pushing " & cfg.envFile & "..."
    discard pushFile(spriteName, cfg.envFile, "/home/sprite/.env", dryRun)

  # ============================================================
  # 5. Push plan file if provided
  # ============================================================
  if planFile != "" and fileExists(planFile):
    echo "Pushing " & planFile & "..."
    discard pushFile(spriteName, planFile, "/home/sprite/plan.md", dryRun)

  # ============================================================
  # 6. Upload directories if provided
  # ============================================================
  for dir in uploadDirs:
    if dirExists(dir):
      let dirName = lastPathPart(dir)
      echo "Uploading directory: " & dir & " -> /home/sprite/" & dirName
      discard pushDir(spriteName, dir, "/home/sprite/" & dirName, dryRun)
    else:
      echo "WARNING: --upload dir '" & dir & "' not found, skipping."

  # ============================================================
  # 7. Setup git + beads
  # ============================================================
  echo "Initializing git..."
  discard sx(spriteName, "cd /home/sprite && git init -b main 2>/dev/null || true", dryRun)

  echo "Installing beads..."
  discard sx(spriteName,
    "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash",
    dryRun)

  # ============================================================
  # 8. Install and auth coding agent
  # ============================================================
  if cfg.agent == "claude":
    echo "Setting up claude..."
    discard sx(spriteName,
      "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code",
      dryRun)

    if cfg.claudeAuth == "subscription":
      let credPath = getHomeDir() / ".claude" / ".credentials.json"
      if fileExists(credPath):
        echo "Copying claude subscription credentials..."
        discard pushFile(spriteName, credPath, "/home/sprite/.claude/.credentials.json", dryRun)
        discard sx(spriteName, "chmod 600 ~/.claude/.credentials.json", dryRun)
      else:
        echo "ERROR: ~/.claude/.credentials.json not found"
        echo "Run 'claude' locally first to authenticate, then re-run this script."
        quit(1)
    elif cfg.claudeAuth == "apikey" and cfg.anthropicApiKey != "":
      echo "Setting ANTHROPIC_API_KEY in sprite..."
      discard sx(spriteName,
        "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"" &
        cfg.anthropicApiKey & "\"' >> ~/.bashrc",
        dryRun)
    else:
      echo "ERROR: No valid claude auth configured"
      echo "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY"
      quit(1)

  elif cfg.agent == "opencode":
    echo "Setting up opencode..."
    discard sx(spriteName,
      "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash",
      dryRun)
    discard sx(spriteName,
      "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc",
      dryRun)
  else:
    echo "ERROR: Unknown AGENT '" & cfg.agent & "'. Use 'claude' or 'opencode'."
    quit(1)

  # ============================================================
  # 9. Launch agent with plan (or open console)
  # ============================================================
  echo ""
  echo "=========================================="
  echo "Sprite '" & spriteName & "' is ready!"
  var agentLine = "Agent: " & cfg.agent
  if cfg.model != "":
    agentLine &= " (model: " & cfg.model & ")"
  echo agentLine
  if checkpointing:
    echo "Checkpointing: every " & $cfg.checkpointInterval & "s"
  echo "=========================================="

  if dryRun:
    echo ""
    echo "[dry-run] Would launch " & cfg.agent & " with plan. No changes were made."
    quit(0)

  if planFile != "":
    # Start auto-checkpointing before agent runs
    if checkpointing:
      startCheckpointing(spriteName, cfg.checkpointInterval)

    echo "Launching " & cfg.agent & " with plan..."

    if cfg.agent == "claude":
      var modelFlag = ""
      if cfg.model != "":
        modelFlag = "--model " & cfg.model & " "
      discard sx(spriteName,
        "cd /home/sprite && claude " & modelFlag & "-p 'read plan.md and complete the plan please'")

    elif cfg.agent == "opencode":
      let ocModel = if cfg.model != "": cfg.model else: "opencode/big-pickle"
      discard sx(spriteName,
        "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m " &
        ocModel & " 'read plan.md and complete the plan please'")

    # Final checkpoint after agent completes
    if checkpointing:
      stopCheckpointing()
    echo "Creating final checkpoint..."
    let (_, exitCode) = spriteCmdCapture("checkpoint", "create", "-s", spriteName)
    if exitCode == 0:
      echo "Final checkpoint saved."
    else:
      echo "Final checkpoint failed (non-fatal)."
  else:
    echo "Opening console..."
    discard spriteCmd("console", "-s", spriteName)
