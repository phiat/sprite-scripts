using System;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace SpriteTool.Commands;

public static class LaunchCommand
{
    public static int Execute(bool dryRun, bool noCheckpoint, string[] uploadDirs, string spriteName, string? planFile)
    {
        var cfg = Config.Load();
        var checkpointing = !noCheckpoint;

        // ============================================================
        // 1. Check/install sprite CLI
        // ============================================================
        if (!SpriteExec.IsSpriteInstalled())
        {
            if (dryRun)
            {
                Console.WriteLine("  [dry-run] Would install sprite CLI");
            }
            else
            {
                SpriteExec.InstallSprite();
            }
        }

        // ============================================================
        // 2. Auth sprite (non-interactive if token provided)
        // ============================================================
        if (!string.IsNullOrEmpty(cfg.SpriteToken))
        {
            Console.WriteLine("Authenticating sprite with token...");
            if (!dryRun)
            {
                SpriteExec.SpriteRun("auth", "setup", "--token", cfg.SpriteToken);
            }
        }
        else
        {
            Console.WriteLine("No SPRITE_TOKEN set. Running interactive login...");
            if (!dryRun)
            {
                SpriteExec.SpriteRun("login");
            }
        }

        // ============================================================
        // 3. Create sprite (or use existing)
        // ============================================================
        if (dryRun)
        {
            Console.WriteLine($"  [dry-run] Would create or reuse sprite '{spriteName}'");
        }
        else
        {
            var (_, lsOutput) = SpriteExec.SpriteCapture("ls");
            if (lsOutput.Split('\n').Any(line => line.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries)
                .Any(word => word == spriteName)))
            {
                Console.WriteLine($"Sprite '{spriteName}' already exists, using it.");
            }
            else
            {
                Console.WriteLine($"Creating sprite: {spriteName}");
                SpriteExec.SpriteRun("create", "-skip-console", spriteName);
            }
        }

        // ============================================================
        // 4. Push .env to sprite
        // ============================================================
        if (File.Exists(cfg.EnvFile))
        {
            Console.WriteLine($"Pushing {cfg.EnvFile}...");
            SpriteExec.PushFile(spriteName, cfg.EnvFile, "/home/sprite/.env", dryRun);
        }

        // ============================================================
        // 5. Push plan file if provided
        // ============================================================
        if (!string.IsNullOrEmpty(planFile) && File.Exists(planFile))
        {
            Console.WriteLine($"Pushing {planFile}...");
            SpriteExec.PushFile(spriteName, planFile, "/home/sprite/plan.md", dryRun);
        }

        // ============================================================
        // 6. Upload directories if provided
        // ============================================================
        if (uploadDirs != null)
        {
            foreach (var dir in uploadDirs)
            {
                if (Directory.Exists(dir))
                {
                    var dirName = Path.GetFileName(Path.GetFullPath(dir));
                    Console.WriteLine($"Uploading directory: {dir} -> /home/sprite/{dirName}");
                    SpriteExec.PushDir(spriteName, dir, $"/home/sprite/{dirName}", dryRun);
                }
                else
                {
                    Console.WriteLine($"WARNING: --upload dir '{dir}' not found, skipping.");
                }
            }
        }

        // ============================================================
        // 7. Setup git + beads
        // ============================================================
        Console.WriteLine("Initializing git...");
        SpriteExec.Sx(spriteName, "cd /home/sprite && git init -b main 2>/dev/null || true", dryRun);

        Console.WriteLine("Installing beads...");
        SpriteExec.Sx(spriteName,
            "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash", dryRun);

        // ============================================================
        // 8. Install and auth coding agent
        // ============================================================
        if (cfg.Agent == "claude")
        {
            Console.WriteLine("Setting up claude...");
            SpriteExec.Sx(spriteName,
                "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code", dryRun);

            if (cfg.ClaudeAuth == "subscription")
            {
                var credPath = Path.Combine(
                    Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                    ".claude", ".credentials.json");
                if (File.Exists(credPath))
                {
                    Console.WriteLine("Copying claude subscription credentials...");
                    SpriteExec.PushFile(spriteName, credPath, "/home/sprite/.claude/.credentials.json", dryRun);
                    SpriteExec.Sx(spriteName, "chmod 600 ~/.claude/.credentials.json", dryRun);
                }
                else
                {
                    Console.Error.WriteLine("ERROR: ~/.claude/.credentials.json not found");
                    Console.Error.WriteLine("Run 'claude' locally first to authenticate, then re-run this script.");
                    return 1;
                }
            }
            else if (cfg.ClaudeAuth == "apikey" && !string.IsNullOrEmpty(cfg.AnthropicApiKey))
            {
                Console.WriteLine("Setting ANTHROPIC_API_KEY in sprite...");
                SpriteExec.Sx(spriteName,
                    $"grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"{cfg.AnthropicApiKey}\"' >> ~/.bashrc",
                    dryRun);
            }
            else
            {
                Console.Error.WriteLine("ERROR: No valid claude auth configured");
                Console.Error.WriteLine("Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY");
                return 1;
            }
        }
        else if (cfg.Agent == "opencode")
        {
            Console.WriteLine("Setting up opencode...");
            SpriteExec.Sx(spriteName,
                "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash", dryRun);
            SpriteExec.Sx(spriteName,
                "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc",
                dryRun);
        }
        else
        {
            Console.Error.WriteLine($"ERROR: Unknown AGENT '{cfg.Agent}'. Use 'claude' or 'opencode'.");
            return 1;
        }

        // ============================================================
        // 9. Launch agent with plan (or open console)
        // ============================================================
        Console.WriteLine();
        Console.WriteLine("==========================================");
        Console.WriteLine($"Sprite '{spriteName}' is ready!");
        var modelSuffix = !string.IsNullOrEmpty(cfg.Model) ? $" (model: {cfg.Model})" : "";
        Console.WriteLine($"Agent: {cfg.Agent}{modelSuffix}");
        if (checkpointing)
            Console.WriteLine($"Checkpointing: every {cfg.CheckpointInterval}s");
        Console.WriteLine("==========================================");

        if (dryRun)
        {
            Console.WriteLine();
            Console.WriteLine($"[dry-run] Would launch {cfg.Agent} with plan. No changes were made.");
            return 0;
        }

        if (!string.IsNullOrEmpty(planFile))
        {
            // Start auto-checkpointing before agent runs
            using var cts = new CancellationTokenSource();
            Task? checkpointTask = null;

            if (checkpointing)
            {
                checkpointTask = StartCheckpointing(spriteName, cfg.CheckpointInterval, cts.Token);
            }

            // Register cleanup on process exit
            AppDomain.CurrentDomain.ProcessExit += (_, _) =>
            {
                cts.Cancel();
            };

            Console.WriteLine($"Launching {cfg.Agent} with plan...");

            if (cfg.Agent == "claude")
            {
                var modelFlag = !string.IsNullOrEmpty(cfg.Model) ? $"--model {cfg.Model} " : "";
                SpriteExec.Sx(spriteName,
                    $"cd /home/sprite && claude {modelFlag}-p 'read plan.md and complete the plan please'");
            }
            else if (cfg.Agent == "opencode")
            {
                var ocModel = !string.IsNullOrEmpty(cfg.Model) ? cfg.Model : "opencode/big-pickle";
                SpriteExec.Sx(spriteName,
                    $"set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m {ocModel} 'read plan.md and complete the plan please'");
            }

            // Stop checkpointing
            cts.Cancel();
            if (checkpointTask != null)
            {
                try { checkpointTask.Wait(); } catch (AggregateException) { }
            }

            // Final checkpoint
            Console.WriteLine("Creating final checkpoint...");
            var (exitCode, _) = SpriteExec.SpriteCapture("checkpoint", "create", "-s", spriteName);
            if (exitCode == 0)
                Console.WriteLine("Final checkpoint saved.");
            else
                Console.WriteLine("Final checkpoint failed (non-fatal).");
        }
        else
        {
            Console.WriteLine("Opening console...");
            SpriteExec.SpriteRun("console", "-s", spriteName);
        }

        return 0;
    }

    private static Task StartCheckpointing(string sprite, int intervalSeconds, CancellationToken ct)
    {
        Console.WriteLine($"Auto-checkpointing every {intervalSeconds}s");
        return Task.Run(async () =>
        {
            while (!ct.IsCancellationRequested)
            {
                try
                {
                    await Task.Delay(TimeSpan.FromSeconds(intervalSeconds), ct);
                }
                catch (OperationCanceledException)
                {
                    break;
                }

                Console.WriteLine($"[checkpoint] Creating checkpoint at {DateTime.Now:HH:mm:ss}...");
                var (exitCode, _) = SpriteExec.SpriteCapture("checkpoint", "create", "-s", sprite);
                if (exitCode == 0)
                    Console.WriteLine("[checkpoint] Done.");
                else
                    Console.WriteLine("[checkpoint] Failed (non-fatal).");
            }
        }, ct);
    }
}
