import sys.io.File;
import sys.FileSystem;
import sys.thread.Thread;

/**
 * Launch implements the sprite-launch subcommand.
 * Creates and configures a sprite with a coding agent, git, and beads.
 *
 * Usage: sprite-tool launch [--dry-run] [--no-checkpoint] [--upload dir] sprite-name [plan-file]
 */
class Launch {
    static var checkpointRunning:Bool = false;

    public static function usage():Void {
        Sys.println("Usage: sprite-tool launch [options] <sprite-name> [plan-file]");
        Sys.println("");
        Sys.println("Options:");
        Sys.println("  --dry-run              Show what would happen without executing");
        Sys.println("  --no-checkpoint        Disable auto-checkpointing");
        Sys.println("  --upload <dir>         Upload a local directory to /home/sprite/<dirname>");
        Sys.println("                         (repeatable: --upload ./data --upload ./tests)");
        Sys.println("");
        Sys.println("Environment variables:");
        Sys.println("  ENV_FILE               Path to .env file (default: ./.env)");
        Sys.println("  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)");
        Sys.println("  AGENT                  \"opencode\" (default) or \"claude\"");
        Sys.println("  CLAUDE_AUTH            \"subscription\" (default) or \"apikey\"");
        Sys.println("  MODEL                  Model override (see below)");
        Sys.println("  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)");
        Sys.println("");
        Sys.println("Model examples:");
        Sys.println("  OpenCode: MODEL=opencode/big-pickle  (free, default)");
        Sys.println("            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)");
        Sys.println("            MODEL=openai/gpt-4o");
        Sys.println("            MODEL=google/gemini-2.5-pro");
        Sys.println("  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku");
        Sys.println("");
        Sys.println("Examples:");
        Sys.println("  sprite-tool launch my-project plan.md");
        Sys.println("  sprite-tool launch --upload ./data my-project plan.md");
        Sys.println("  sprite-tool launch --upload ./data --upload ./tests my-project plan.md");
        Sys.println("  sprite-tool launch --dry-run my-project plan.md");
    }

    public static function run(args:Array<String>):Void {
        // ============================
        // Configuration
        // ============================
        var envFile = Config.getEnvOr("ENV_FILE", "./.env");

        // Load .env first
        Config.loadEnv(envFile);

        // After loading .env, apply SPRITES_TOKEN fallback
        var spriteToken = Config.getEnvOrEmpty("SPRITE_TOKEN");
        if (spriteToken.length == 0) {
            spriteToken = Config.getEnvOrEmpty("SPRITES_TOKEN");
            if (spriteToken.length > 0) {
                Sys.putEnv("SPRITE_TOKEN", spriteToken);
            }
        }

        var agent = Config.getEnvOr("AGENT", "opencode");
        var claudeAuth = Config.getEnvOr("CLAUDE_AUTH", "subscription");
        var anthropicApiKey = Config.getEnvOrEmpty("ANTHROPIC_API_KEY");
        var model = Config.getEnvOrEmpty("MODEL");
        var checkpointIntervalStr = Config.getEnvOr("CHECKPOINT_INTERVAL", "300");
        var checkpointInterval:Float = Std.parseFloat(checkpointIntervalStr);
        if (Math.isNaN(checkpointInterval) || checkpointInterval <= 0) {
            checkpointInterval = 300;
        }

        // ============================
        // Parse flags
        // ============================
        var dryRun = false;
        var checkpointing = true;
        var uploadDirs:Array<String> = [];
        var positional:Array<String> = [];

        var i = 0;
        while (i < args.length) {
            var arg = args[i];
            if (arg == "--dry-run") {
                dryRun = true;
            } else if (arg == "--no-checkpoint") {
                checkpointing = false;
            } else if (arg == "--upload") {
                i++;
                if (i < args.length) {
                    uploadDirs.push(args[i]);
                } else {
                    Sys.println("Error: --upload requires an argument");
                    usage();
                    Sys.exit(1);
                }
            } else if (arg == "--help" || arg == "-h") {
                usage();
                Sys.exit(0);
            } else if (StringTools.startsWith(arg, "--")) {
                Sys.println('Unknown option: $arg');
                usage();
                Sys.exit(1);
            } else {
                positional.push(arg);
            }
            i++;
        }

        if (positional.length < 1) {
            usage();
            Sys.exit(1);
        }

        var spriteName = positional[0];
        var planFile = if (positional.length > 1) positional[1] else "";

        // ============================
        // 1. Check/install sprite CLI
        // ============================
        if (Sys.command("bash", ["-c", "command -v sprite >/dev/null 2>&1"]) != 0) {
            if (dryRun) {
                Sys.println("  [dry-run] Would install sprite CLI");
            } else {
                Sys.println("Installing sprite CLI...");
                Sys.command("bash", ["-c", "curl -fsSL https://sprites.dev/install.sh | sh"]);
                // Update PATH
                var home = Config.getEnvOrEmpty("HOME");
                var path = Config.getEnvOrEmpty("PATH");
                Sys.putEnv("PATH", '$home/.local/bin:$path');
            }
        }

        // ============================
        // 2. Auth sprite
        // ============================
        if (spriteToken.length > 0) {
            Sys.println("Authenticating sprite with token...");
            if (!dryRun) {
                Sys.command("sprite", ["auth", "setup", "--token", spriteToken]);
            }
        } else {
            Sys.println("No SPRITE_TOKEN set. Running interactive login...");
            if (!dryRun) {
                Sys.command("sprite", ["login"]);
            }
        }

        // ============================
        // 3. Create sprite (or use existing)
        // ============================
        if (dryRun) {
            Sys.println('  [dry-run] Would create or reuse sprite \'$spriteName\'');
        } else {
            // Check if sprite already exists
            var proc = new sys.io.Process("sprite", ["ls"]);
            var lsOutput = proc.stdout.readAll().toString();
            proc.exitCode();
            proc.close();
            if (lsOutput.indexOf(spriteName) >= 0) {
                Sys.println('Sprite \'$spriteName\' already exists, using it.');
            } else {
                Sys.println('Creating sprite: $spriteName');
                Sys.command("sprite", ["create", "-skip-console", spriteName]);
            }
        }

        // ============================
        // 4. Push .env to sprite
        // ============================
        if (FileSystem.exists(envFile)) {
            Sys.println('Pushing $envFile...');
            Sprite.pushFile(spriteName, envFile, "/home/sprite/.env", dryRun);
        }

        // ============================
        // 5. Push plan file if provided
        // ============================
        if (planFile.length > 0 && FileSystem.exists(planFile)) {
            Sys.println('Pushing $planFile...');
            Sprite.pushFile(spriteName, planFile, "/home/sprite/plan.md", dryRun);
        }

        // ============================
        // 6. Upload directories if provided
        // ============================
        for (dir in uploadDirs) {
            if (FileSystem.exists(dir) && FileSystem.isDirectory(dir)) {
                var dirName = Sprite.basename(dir);
                Sys.println('Uploading directory: $dir -> /home/sprite/$dirName');
                Sprite.pushDir(spriteName, dir, '/home/sprite/$dirName', dryRun);
            } else {
                Sys.println('WARNING: --upload dir \'$dir\' not found, skipping.');
            }
        }

        // ============================
        // 7. Setup git + beads
        // ============================
        Sys.println("Initializing git...");
        Sprite.sx(spriteName, "cd /home/sprite && git init -b main 2>/dev/null || true", dryRun);

        Sys.println("Installing beads...");
        Sprite.sx(spriteName, "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash", dryRun);

        // ============================
        // 8. Install and auth coding agent
        // ============================
        if (agent == "claude") {
            Sys.println("Setting up claude...");
            Sprite.sx(spriteName, "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code", dryRun);

            if (claudeAuth == "subscription") {
                var home = Config.getEnvOrEmpty("HOME");
                var credFile = '$home/.claude/.credentials.json';
                if (FileSystem.exists(credFile)) {
                    Sys.println("Copying claude subscription credentials...");
                    Sprite.pushFile(spriteName, credFile, "/home/sprite/.claude/.credentials.json", dryRun);
                    Sprite.sx(spriteName, "chmod 600 ~/.claude/.credentials.json", dryRun);
                } else {
                    Sys.println("ERROR: ~/.claude/.credentials.json not found");
                    Sys.println("Run 'claude' locally first to authenticate, then re-run this script.");
                    Sys.exit(1);
                }
            } else if (claudeAuth == "apikey" && anthropicApiKey.length > 0) {
                Sys.println("Setting ANTHROPIC_API_KEY in sprite...");
                Sprite.sx(spriteName, 'grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo \'export ANTHROPIC_API_KEY="$anthropicApiKey"\' >> ~/.bashrc', dryRun);
            } else {
                Sys.println("ERROR: No valid claude auth configured");
                Sys.println("Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY");
                Sys.exit(1);
            }
        } else if (agent == "opencode") {
            Sys.println("Setting up opencode...");
            Sprite.sx(spriteName, "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash", dryRun);
            Sprite.sx(spriteName, "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc", dryRun);
        } else {
            Sys.println('ERROR: Unknown AGENT \'$agent\'. Use \'claude\' or \'opencode\'.');
            Sys.exit(1);
        }

        // ============================
        // 9. Launch agent with plan (or open console)
        // ============================
        Sys.println("");
        Sys.println("==========================================");
        Sys.println('Sprite \'$spriteName\' is ready!');
        var modelStr = if (model.length > 0) ' (model: $model)' else "";
        Sys.println('Agent: $agent$modelStr');
        if (checkpointing) {
            Sys.println('Checkpointing: every ${Std.string(Std.int(checkpointInterval))}s');
        }
        Sys.println("==========================================");

        if (dryRun) {
            Sys.println("");
            Sys.println('[dry-run] Would launch $agent with plan. No changes were made.');
            Sys.exit(0);
        }

        if (planFile.length > 0) {
            // Start auto-checkpointing before agent runs
            if (checkpointing) {
                checkpointRunning = true;
                Sys.println('Auto-checkpointing every ${Std.string(Std.int(checkpointInterval))}s');
                Thread.create(function() {
                    while (checkpointRunning) {
                        Sys.sleep(checkpointInterval);
                        if (!checkpointRunning) break;
                        Sys.println("[checkpoint] Creating checkpoint...");
                        var rc = Sys.command("sprite", ["checkpoint", "create", "-s", spriteName]);
                        if (rc == 0) {
                            Sys.println("[checkpoint] Done.");
                        } else {
                            Sys.println("[checkpoint] Failed (non-fatal).");
                        }
                    }
                });
            }

            Sys.println('Launching $agent with plan...');

            if (agent == "claude") {
                var modelFlag = if (model.length > 0) '--model $model' else "";
                Sprite.sx(spriteName, 'cd /home/sprite && claude $modelFlag -p \'read plan.md and complete the plan please\'', false);
            } else if (agent == "opencode") {
                var ocModel = if (model.length > 0) model else "opencode/big-pickle";
                Sprite.sx(spriteName, 'set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m $ocModel \'read plan.md and complete the plan please\'', false);
            }

            // Stop checkpointing
            checkpointRunning = false;

            // Final checkpoint
            Sys.println("Creating final checkpoint...");
            var rc = Sys.command("sprite", ["checkpoint", "create", "-s", spriteName]);
            if (rc == 0) {
                Sys.println("Final checkpoint saved.");
            } else {
                Sys.println("Final checkpoint failed (non-fatal).");
            }
        } else {
            Sys.println("Opening console...");
            Sys.command("sprite", ["console", "-s", spriteName]);
        }
    }
}
