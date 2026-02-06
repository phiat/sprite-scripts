module launch;

import std.conv : to;
import std.file : exists, isDir;
import std.format : format;
import std.path : absolutePath, baseName;
import std.process : environment, executeShell, spawnShell, wait;
import std.stdio : writeln, writefln, stderr;
import std.string : startsWith;

import config : Config, loadConfig;
import sprite;

import core.thread : Thread;
import core.time : dur;

private void usage()
{
    writeln(
        "Usage: sprite-tool launch [options] <sprite-name> [plan-file]\n"
        ~ "\n"
        ~ "Options:\n"
        ~ "  --dry-run              Show what would happen without executing\n"
        ~ "  --no-checkpoint        Disable auto-checkpointing\n"
        ~ "  --upload <dir>         Upload a local directory to /home/sprite/<dirname>\n"
        ~ "                         (repeatable: --upload ./data --upload ./tests)\n"
        ~ "\n"
        ~ "Environment variables:\n"
        ~ "  ENV_FILE               Path to .env file (default: ./.env)\n"
        ~ "  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)\n"
        ~ "  AGENT                  \"opencode\" (default) or \"claude\"\n"
        ~ "  CLAUDE_AUTH            \"subscription\" (default) or \"apikey\"\n"
        ~ "  MODEL                  Model override (see below)\n"
        ~ "  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)\n"
        ~ "\n"
        ~ "Model examples:\n"
        ~ "  OpenCode: MODEL=opencode/big-pickle  (free, default)\n"
        ~ "            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)\n"
        ~ "            MODEL=openai/gpt-4o\n"
        ~ "            MODEL=google/gemini-2.5-pro\n"
        ~ "  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku\n"
        ~ "\n"
        ~ "Examples:\n"
        ~ "  sprite-tool launch my-project plan.md\n"
        ~ "  sprite-tool launch --upload ./data my-project plan.md\n"
        ~ "  sprite-tool launch --upload ./data --upload ./tests my-project plan.md\n"
        ~ "  sprite-tool launch --dry-run my-project plan.md\n"
        ~ "  MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md\n"
        ~ "  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md"
    );

    import core.stdc.stdlib : exit;
    exit(1);
}

/// Background checkpoint thread state.
private __gshared bool checkpointRunning = false;

/// Background checkpoint loop thread function.
private void runCheckpointLoop(string spriteName, int interval)
{
    import std.datetime : Clock;

    while (checkpointRunning)
    {
        Thread.sleep(dur!"seconds"(interval));
        if (!checkpointRunning)
            break;

        auto now = Clock.currTime();
        writefln("[checkpoint] Creating checkpoint at %02d:%02d:%02d...",
            now.hour, now.minute, now.second);

        auto result = executeShell(
            format!"sprite checkpoint create -s %s 2>/dev/null"(spriteName));
        if (result.status == 0)
            writeln("[checkpoint] Done.");
        else
            writeln("[checkpoint] Failed (non-fatal).");
    }
}

void run(string[] args)
{
    // Load config from .env + environment
    Config cfg = loadConfig();

    // Parse flags
    bool dryRun = false;
    bool checkpointing = true;
    string[] uploadDirs;
    string[] positional;

    size_t i = 0;
    while (i < args.length)
    {
        string arg = args[i];
        if (arg == "--dry-run")
        {
            dryRun = true;
            i++;
        }
        else if (arg == "--no-checkpoint")
        {
            checkpointing = false;
            i++;
        }
        else if (arg == "--upload")
        {
            i++;
            if (i >= args.length)
            {
                stderr.writeln("Error: --upload requires an argument");
                import core.stdc.stdlib : exit;
                exit(1);
            }
            uploadDirs ~= args[i];
            i++;
        }
        else if (arg == "--help" || arg == "-h")
        {
            usage();
        }
        else if (arg.startsWith("--"))
        {
            stderr.writefln("Unknown option: %s", arg);
            usage();
        }
        else
        {
            positional ~= arg;
            i++;
        }
    }

    if (positional.length < 1)
        usage();

    string spriteName = positional[0];
    string planFile = positional.length > 1 ? positional[1] : "";

    // 1. Check/install sprite CLI
    auto whichResult = executeShell("command -v sprite 2>/dev/null");
    if (whichResult.status != 0)
    {
        if (dryRun)
        {
            writeln("  [dry-run] Would install sprite CLI");
        }
        else
        {
            writeln("Installing sprite CLI...");
            executeShell("curl -fsSL https://sprites.dev/install.sh | sh");

            string home = environment.get("HOME", "");
            string currentPath = environment.get("PATH", "");
            environment["PATH"] = home ~ "/.local/bin:" ~ currentPath;
        }
    }

    // 2. Auth sprite
    if (cfg.spriteToken.length > 0)
    {
        writeln("Authenticating sprite with token...");
        if (!dryRun)
        {
            executeShell(format!"sprite auth setup --token %s"(
                sprite.shellQuote(cfg.spriteToken)));
        }
    }
    else
    {
        writeln("No SPRITE_TOKEN set. Running interactive login...");
        if (!dryRun)
        {
            auto pid = spawnShell("sprite login");
            pid.wait();
        }
    }

    // 3. Create sprite (or use existing)
    if (dryRun)
    {
        writefln("  [dry-run] Would create or reuse sprite '%s'", spriteName);
    }
    else if (sprite.spriteExists(spriteName))
    {
        writefln("Sprite '%s' already exists, using it.", spriteName);
    }
    else
    {
        writefln("Creating sprite: %s", spriteName);
        executeShell(format!"sprite create -skip-console %s"(
            sprite.shellQuote(spriteName)));
    }

    // 4. Push .env to sprite
    if (exists(cfg.envFile))
    {
        writefln("Pushing %s...", cfg.envFile);
        sprite.pushFile(spriteName, cfg.envFile, "/home/sprite/.env", dryRun);
    }

    // 5. Push plan file if provided
    if (planFile.length > 0 && exists(planFile))
    {
        writefln("Pushing %s...", planFile);
        sprite.pushFile(spriteName, planFile, "/home/sprite/plan.md", dryRun);
    }

    // 6. Upload directories if provided
    foreach (dir; uploadDirs)
    {
        if (exists(dir) && isDir(dir))
        {
            string dirName = baseName(absolutePath(dir));
            writefln("Uploading directory: %s -> /home/sprite/%s", dir, dirName);
            sprite.pushDir(spriteName, dir, format!"/home/sprite/%s"(dirName), dryRun);
        }
        else
        {
            writefln("WARNING: --upload dir '%s' not found, skipping.", dir);
        }
    }

    // 7. Setup git + beads
    writeln("Initializing git...");
    sprite.sx(spriteName, "cd /home/sprite && git init -b main 2>/dev/null || true", dryRun);

    writeln("Installing beads...");
    sprite.sx(spriteName,
        "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash",
        dryRun);

    // 8. Install and auth coding agent
    if (cfg.agent == "claude")
    {
        writeln("Setting up claude...");
        sprite.sx(spriteName,
            "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code",
            dryRun);

        if (cfg.claudeAuth == "subscription")
        {
            string home = environment.get("HOME", "");
            string credsPath = home ~ "/.claude/.credentials.json";
            if (exists(credsPath))
            {
                writeln("Copying claude subscription credentials...");
                sprite.pushFile(spriteName, credsPath,
                    "/home/sprite/.claude/.credentials.json", dryRun);
                sprite.sx(spriteName, "chmod 600 ~/.claude/.credentials.json", dryRun);
            }
            else
            {
                stderr.writeln("ERROR: ~/.claude/.credentials.json not found");
                stderr.writeln(
                    "Run 'claude' locally first to authenticate, then re-run this script.");
                import core.stdc.stdlib : exit;
                exit(1);
            }
        }
        else if (cfg.claudeAuth == "apikey" && cfg.anthropicApiKey.length > 0)
        {
            writeln("Setting ANTHROPIC_API_KEY in sprite...");
            sprite.sx(spriteName,
                format!"grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"%s\"' >> ~/.bashrc"(
                    cfg.anthropicApiKey),
                dryRun);
        }
        else
        {
            stderr.writeln("ERROR: No valid claude auth configured");
            stderr.writeln(
                "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY");
            import core.stdc.stdlib : exit;
            exit(1);
        }
    }
    else if (cfg.agent == "opencode")
    {
        writeln("Setting up opencode...");
        sprite.sx(spriteName,
            "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash",
            dryRun);

        sprite.sx(spriteName,
            `grep -q 'source.*\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc`,
            dryRun);
    }
    else
    {
        stderr.writefln("ERROR: Unknown AGENT '%s'. Use 'claude' or 'opencode'.", cfg.agent);
        import core.stdc.stdlib : exit;
        exit(1);
    }

    // 9. Launch agent with plan (or open console)
    writeln("");
    writeln("==========================================");
    writefln("Sprite '%s' is ready!", spriteName);

    string modelNote = cfg.model.length > 0
        ? format!" (model: %s)"(cfg.model) : "";
    writefln("Agent: %s%s", cfg.agent, modelNote);

    if (checkpointing)
        writefln("Checkpointing: every %ds", cfg.checkpointInterval);

    writeln("==========================================");

    if (dryRun)
    {
        writeln("");
        writefln("[dry-run] Would launch %s with plan. No changes were made.", cfg.agent);
        return;
    }

    if (planFile.length > 0)
    {
        // Start auto-checkpointing before agent runs
        Thread checkpointThread;
        if (checkpointing)
        {
            checkpointRunning = true;
            // Capture values for the thread
            string sName = spriteName;
            int interval = cfg.checkpointInterval;
            checkpointThread = new Thread(() => runCheckpointLoop(sName, interval));
            checkpointThread.isDaemon = true;
            checkpointThread.start();
            writefln("Auto-checkpointing every %ds", cfg.checkpointInterval);
        }

        writefln("Launching %s with plan...", cfg.agent);

        if (cfg.agent == "claude")
        {
            string modelFlag = cfg.model.length > 0
                ? format!"--model %s "(cfg.model) : "";
            sprite.sxPassthrough(spriteName,
                format!"cd /home/sprite && claude %s-p 'read plan.md and complete the plan please'"(
                    modelFlag),
                false);
        }
        else if (cfg.agent == "opencode")
        {
            string ocModel = cfg.model.length > 0
                ? cfg.model : "opencode/big-pickle";
            sprite.sxPassthrough(spriteName,
                format!"set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m %s 'read plan.md and complete the plan please'"(
                    ocModel),
                false);
        }

        // Stop checkpoint loop
        checkpointRunning = false;

        // Final checkpoint after agent completes
        writeln("Creating final checkpoint...");
        auto result = executeShell(
            format!"sprite checkpoint create -s %s 2>/dev/null"(spriteName));
        if (result.status == 0)
            writeln("Final checkpoint saved.");
        else
            writeln("Final checkpoint failed (non-fatal).");
    }
    else
    {
        writeln("Opening console...");
        auto pid = spawnShell(format!"sprite console -s %s"(
            sprite.shellQuote(spriteName)));
        pid.wait();
    }
}
