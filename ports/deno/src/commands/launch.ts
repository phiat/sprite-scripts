/**
 * launch.ts — Create and configure a sprite with coding agent, git, beads
 *
 * Usage: sprite-tool launch [--dry-run] [--no-checkpoint] [--upload dir] sprite-name [plan-file]
 */

import { loadConfig, type Config } from "../lib/config.ts";
import {
  sx,
  spriteCmd,
  isSpriteInstalled,
  installSprite,
} from "../lib/sprite.ts";
import { pushFile, pushDir } from "../lib/transfer.ts";
import {
  startCheckpointing,
  stopCheckpointing,
  createCheckpoint,
} from "../lib/checkpoint.ts";

function usage(): never {
  console.log(`Usage: sprite-tool launch [options] <sprite-name> [plan-file]

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
  sprite-tool launch --dry-run my-project plan.md`);
  Deno.exit(1);
}

interface LaunchArgs {
  dryRun: boolean;
  checkpointing: boolean;
  uploadDirs: string[];
  spriteName: string;
  planFile: string;
}

function parseArgs(args: string[]): LaunchArgs {
  let dryRun = false;
  let checkpointing = true;
  const uploadDirs: string[] = [];
  const positional: string[] = [];

  let i = 0;
  while (i < args.length) {
    const arg = args[i];
    if (arg === "--dry-run") {
      dryRun = true;
      i++;
    } else if (arg === "--no-checkpoint") {
      checkpointing = false;
      i++;
    } else if (arg === "--upload") {
      i++;
      if (i >= args.length) {
        console.error("Error: --upload requires an argument");
        usage();
      }
      uploadDirs.push(args[i]);
      i++;
    } else if (arg === "--help" || arg === "-h") {
      usage();
    } else if (arg.startsWith("--")) {
      console.error(`Unknown option: ${arg}`);
      usage();
    } else {
      positional.push(arg);
      i++;
    }
  }

  if (positional.length < 1) {
    usage();
  }

  return {
    dryRun,
    checkpointing,
    uploadDirs,
    spriteName: positional[0],
    planFile: positional[1] || "",
  };
}

function fileExists(path: string): boolean {
  try {
    Deno.statSync(path);
    return true;
  } catch {
    return false;
  }
}

function isDirectory(path: string): boolean {
  try {
    return Deno.statSync(path).isDirectory;
  } catch {
    return false;
  }
}

/**
 * Return the last path component (pure string, no I/O).
 */
function basename(p: string): string {
  const parts = p.replace(/\/+$/, "").split("/");
  return parts[parts.length - 1] || p;
}

export async function launch(args: string[]): Promise<void> {
  const parsed = parseArgs(args);
  const cfg: Config = loadConfig();

  const { dryRun, checkpointing, uploadDirs, spriteName, planFile } = parsed;

  // Ensure cleanup on exit
  addEventListener("unload", () => {
    stopCheckpointing();
  });

  // ----------------------------------------------------------
  // 1. Check/install sprite CLI
  // ----------------------------------------------------------
  if (!isSpriteInstalled()) {
    if (dryRun) {
      console.log("  [dry-run] Would install sprite CLI");
    } else {
      console.log("Installing sprite CLI...");
      await installSprite();
    }
  }

  // ----------------------------------------------------------
  // 2. Auth sprite
  // ----------------------------------------------------------
  if (cfg.spriteToken) {
    console.log("Authenticating sprite with token...");
    if (!dryRun) {
      spriteCmd(["auth", "setup", "--token", cfg.spriteToken]);
    }
  } else {
    console.log("No SPRITE_TOKEN set. Running interactive login...");
    if (!dryRun) {
      spriteCmd(["login"]);
    }
  }

  // ----------------------------------------------------------
  // 3. Create sprite (or reuse existing)
  // ----------------------------------------------------------
  if (dryRun) {
    console.log(`  [dry-run] Would create or reuse sprite '${spriteName}'`);
  } else {
    const listing = spriteCmd(["ls"], { ignoreError: true });
    // Check if sprite name appears as a whole word in the listing
    const regex = new RegExp(`\\b${spriteName}\\b`);
    if (regex.test(listing)) {
      console.log(`Sprite '${spriteName}' already exists, using it.`);
    } else {
      console.log(`Creating sprite: ${spriteName}`);
      spriteCmd(["create", "-skip-console", spriteName]);
    }
  }

  // ----------------------------------------------------------
  // 4. Push .env to sprite
  // ----------------------------------------------------------
  if (fileExists(cfg.envFile)) {
    console.log(`Pushing ${cfg.envFile}...`);
    await pushFile(spriteName, cfg.envFile, "/home/sprite/.env", dryRun);
  }

  // ----------------------------------------------------------
  // 5. Push plan file if provided
  // ----------------------------------------------------------
  if (planFile && fileExists(planFile)) {
    console.log(`Pushing ${planFile}...`);
    await pushFile(spriteName, planFile, "/home/sprite/plan.md", dryRun);
  }

  // ----------------------------------------------------------
  // 6. Upload directories if provided
  // ----------------------------------------------------------
  for (const dir of uploadDirs) {
    if (isDirectory(dir)) {
      const dirName = basename(dir);
      console.log(`Uploading directory: ${dir} -> /home/sprite/${dirName}`);
      await pushDir(spriteName, dir, `/home/sprite/${dirName}`, dryRun);
    } else {
      console.log(`WARNING: --upload dir '${dir}' not found, skipping.`);
    }
  }

  // ----------------------------------------------------------
  // 7. Setup git + beads
  // ----------------------------------------------------------
  console.log("Initializing git...");
  sx(spriteName, "cd /home/sprite && git init -b main 2>/dev/null || true", {
    dryRun,
    ignoreError: true,
  });

  console.log("Installing beads...");
  sx(
    spriteName,
    "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash",
    { dryRun },
  );

  // ----------------------------------------------------------
  // 8. Install and auth coding agent
  // ----------------------------------------------------------
  if (cfg.agent === "claude") {
    console.log("Setting up claude...");
    sx(
      spriteName,
      "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code",
      { dryRun },
    );

    if (cfg.claudeAuth === "subscription") {
      const home = Deno.env.get("HOME") || "";
      const credsPath = `${home}/.claude/.credentials.json`;
      if (fileExists(credsPath)) {
        console.log("Copying claude subscription credentials...");
        await pushFile(
          spriteName,
          credsPath,
          "/home/sprite/.claude/.credentials.json",
          dryRun,
        );
        sx(spriteName, "chmod 600 ~/.claude/.credentials.json", { dryRun });
      } else {
        console.error("ERROR: ~/.claude/.credentials.json not found");
        console.error(
          "Run 'claude' locally first to authenticate, then re-run this script.",
        );
        Deno.exit(1);
      }
    } else if (cfg.claudeAuth === "apikey" && cfg.anthropicApiKey) {
      console.log("Setting ANTHROPIC_API_KEY in sprite...");
      sx(
        spriteName,
        `grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY="${cfg.anthropicApiKey}"' >> ~/.bashrc`,
        { dryRun },
      );
    } else {
      console.error("ERROR: No valid claude auth configured");
      console.error(
        "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY",
      );
      Deno.exit(1);
    }
  } else if (cfg.agent === "opencode") {
    console.log("Setting up opencode...");
    sx(
      spriteName,
      "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash",
      { dryRun },
    );
    sx(
      spriteName,
      `grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc`,
      { dryRun },
    );
  } else {
    console.error(
      `ERROR: Unknown AGENT '${cfg.agent}'. Use 'claude' or 'opencode'.`,
    );
    Deno.exit(1);
  }

  // ----------------------------------------------------------
  // 9. Launch agent with plan (or open console)
  // ----------------------------------------------------------
  console.log("");
  console.log("==========================================");
  console.log(`Sprite '${spriteName}' is ready!`);
  const modelSuffix = cfg.model ? ` (model: ${cfg.model})` : "";
  console.log(`Agent: ${cfg.agent}${modelSuffix}`);
  if (checkpointing) {
    console.log(`Checkpointing: every ${cfg.checkpointInterval}s`);
  }
  console.log("==========================================");

  if (dryRun) {
    console.log("");
    console.log(
      `[dry-run] Would launch ${cfg.agent} with plan. No changes were made.`,
    );
    Deno.exit(0);
  }

  if (planFile) {
    // Start auto-checkpointing before agent runs
    if (checkpointing) {
      startCheckpointing(spriteName, cfg.checkpointInterval);
    }

    console.log(`Launching ${cfg.agent} with plan...`);

    if (cfg.agent === "claude") {
      const modelFlag = cfg.model ? `--model ${cfg.model} ` : "";
      sx(
        spriteName,
        `cd /home/sprite && claude ${modelFlag}-p 'read plan.md and complete the plan please'`,
      );
    } else if (cfg.agent === "opencode") {
      const ocModel = cfg.model || "opencode/big-pickle";
      sx(
        spriteName,
        `set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m ${ocModel} 'read plan.md and complete the plan please'`,
      );
    }

    // Final checkpoint after agent completes
    stopCheckpointing();
    console.log("Creating final checkpoint...");
    const ok = await createCheckpoint(spriteName);
    if (ok) {
      console.log("Final checkpoint saved.");
    } else {
      console.log("Final checkpoint failed (non-fatal).");
    }
  } else {
    console.log("Opening console...");
    // Interactive console — inherit all stdio
    const proc = new Deno.Command("sprite", {
      args: ["console", "-s", spriteName],
      stdin: "inherit",
      stdout: "inherit",
      stderr: "inherit",
    }).spawn();
    const status = await proc.status;
    if (!status.success) {
      Deno.exit(status.code);
    }
  }
}
