package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:thread"
import "core:time"
import "core:sync"

// Shared state for the checkpoint thread
Checkpoint_State :: struct {
    sprite_name:  string,
    interval_sec: int,
    stop:         bool,
    mutex:        sync.Mutex,
}

// Global checkpoint state (used by the background thread)
g_checkpoint_state: Checkpoint_State

checkpoint_thread_proc :: proc() {
    for {
        // Sleep in 1-second increments so we can check the stop flag
        for i := 0; i < g_checkpoint_state.interval_sec; i += 1 {
            time.sleep(1 * time.Second)
            sync.mutex_lock(&g_checkpoint_state.mutex)
            should_stop := g_checkpoint_state.stop
            sync.mutex_unlock(&g_checkpoint_state.mutex)
            if should_stop {
                return
            }
        }

        sync.mutex_lock(&g_checkpoint_state.mutex)
        should_stop := g_checkpoint_state.stop
        sync.mutex_unlock(&g_checkpoint_state.mutex)
        if should_stop {
            return
        }

        fmt.println("[checkpoint] Creating checkpoint...")
        cmd := fmt.tprintf("sprite checkpoint create -s %s 2>/dev/null", g_checkpoint_state.sprite_name)
        ret := run_cmd(cmd)
        if ret == 0 {
            fmt.println("[checkpoint] Done.")
        } else {
            fmt.println("[checkpoint] Failed (non-fatal).")
        }
    }
}

cmd_launch :: proc(args: []string) {
    // Parse flags
    dry_run := false
    checkpointing := true
    upload_dirs: [dynamic]string
    sprite_name := ""
    plan_file := ""

    i := 0
    for i < len(args) {
        arg := args[i]
        if arg == "--dry-run" {
            dry_run = true
            i += 1
        } else if arg == "--no-checkpoint" {
            checkpointing = false
            i += 1
        } else if arg == "--upload" {
            i += 1
            if i >= len(args) {
                fmt.eprintln("Error: --upload requires an argument")
                launch_usage()
                os.exit(1)
            }
            append(&upload_dirs, args[i])
            i += 1
        } else if arg == "--help" || arg == "-h" {
            launch_usage()
            os.exit(0)
        } else if strings.has_prefix(arg, "--") {
            fmt.eprintf("Unknown option: %s\n", arg)
            launch_usage()
            os.exit(1)
        } else {
            // Positional args: sprite-name [plan-file]
            if sprite_name == "" {
                sprite_name = arg
            } else if plan_file == "" {
                plan_file = arg
            } else {
                fmt.eprintln("Too many positional arguments")
                launch_usage()
                os.exit(1)
            }
            i += 1
        }
    }

    if sprite_name == "" {
        launch_usage()
        os.exit(1)
    }

    // Load .env file
    env_file := get_env_or("ENV_FILE", "./.env")
    load_env_file(env_file)
    apply_sprite_token_fallback()

    // Build config from environment (after .env is loaded)
    cfg := build_config()

    // 1. Check/install sprite CLI
    if !is_sprite_installed() {
        if dry_run {
            fmt.println("  [dry-run] Would install sprite CLI")
        } else {
            fmt.println("Installing sprite CLI...")
            run_cmd("curl -fsSL https://sprites.dev/install.sh | sh")
        }
    }

    // 2. Auth sprite
    if cfg.sprite_token != "" {
        fmt.println("Authenticating sprite with token...")
        if !dry_run {
            auth_cmd := fmt.tprintf("sprite auth setup --token %s", cfg.sprite_token)
            run_cmd(auth_cmd)
        }
    } else {
        fmt.println("No SPRITE_TOKEN set. Running interactive login...")
        if !dry_run {
            run_cmd("sprite login")
        }
    }

    // 3. Create sprite (or use existing)
    if dry_run {
        fmt.printf("  [dry-run] Would create or reuse sprite '%s'\n", sprite_name)
    } else {
        // Check if sprite already exists
        ls_output := run_capture("sprite ls 2>/dev/null")
        if strings.contains(ls_output, sprite_name) {
            fmt.printf("Sprite '%s' already exists, using it.\n", sprite_name)
        } else {
            fmt.printf("Creating sprite: %s\n", sprite_name)
            create_cmd := fmt.tprintf("sprite create -skip-console %s", sprite_name)
            run_cmd(create_cmd)
        }
    }

    // 4. Push .env to sprite
    if file_exists(cfg.env_file) {
        fmt.printf("Pushing %s...\n", cfg.env_file)
        push_file(sprite_name, cfg.env_file, "/home/sprite/.env", dry_run)
    }

    // 5. Push plan file if provided
    if plan_file != "" && file_exists(plan_file) {
        fmt.printf("Pushing %s...\n", plan_file)
        push_file(sprite_name, plan_file, "/home/sprite/plan.md", dry_run)
    }

    // 6. Upload directories if provided
    for dir in upload_dirs {
        if is_directory(dir) {
            dirname := base_name(dir)
            remote := fmt.tprintf("/home/sprite/%s", dirname)
            fmt.printf("Uploading directory: %s -> %s\n", dir, remote)
            push_dir(sprite_name, dir, remote, dry_run)
        } else {
            fmt.printf("WARNING: --upload dir '%s' not found, skipping.\n", dir)
        }
    }

    // 7. Setup git + beads
    fmt.println("Initializing git...")
    sx(sprite_name, "cd /home/sprite && git init -b main 2>/dev/null || true", dry_run)

    fmt.println("Installing beads...")
    sx(sprite_name, "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash", dry_run)

    // 8. Install and auth coding agent
    if cfg.agent == "claude" {
        fmt.println("Setting up claude...")
        sx(sprite_name, "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code", dry_run)

        if cfg.claude_auth == "subscription" {
            home := get_env_or("HOME", "/root")
            cred_path := fmt.tprintf("%s/.claude/.credentials.json", home)
            if file_exists(cred_path) {
                fmt.println("Copying claude subscription credentials...")
                push_file(sprite_name, cred_path, "/home/sprite/.claude/.credentials.json", dry_run)
                sx(sprite_name, "chmod 600 ~/.claude/.credentials.json", dry_run)
            } else {
                fmt.eprintln("ERROR: ~/.claude/.credentials.json not found")
                fmt.eprintln("Run 'claude' locally first to authenticate, then re-run this script.")
                os.exit(1)
            }
        } else if cfg.claude_auth == "apikey" && cfg.anthropic_api_key != "" {
            fmt.println("Setting ANTHROPIC_API_KEY in sprite...")
            apikey_cmd := fmt.tprintf(
                "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"%s\"' >> ~/.bashrc",
                cfg.anthropic_api_key,
            )
            sx(sprite_name, apikey_cmd, dry_run)
        } else {
            fmt.eprintln("ERROR: No valid claude auth configured")
            fmt.eprintln("Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY")
            os.exit(1)
        }
    } else if cfg.agent == "opencode" {
        fmt.println("Setting up opencode...")
        sx(sprite_name, "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash", dry_run)
        sx(sprite_name, "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc", dry_run)
    } else {
        fmt.eprintf("ERROR: Unknown AGENT '%s'. Use 'claude' or 'opencode'.\n", cfg.agent)
        os.exit(1)
    }

    // 9. Launch agent with plan (or open console)
    fmt.println("")
    fmt.println("==========================================")
    fmt.printf("Sprite '%s' is ready!\n", sprite_name)
    if cfg.model != "" {
        fmt.printf("Agent: %s (model: %s)\n", cfg.agent, cfg.model)
    } else {
        fmt.printf("Agent: %s\n", cfg.agent)
    }
    if checkpointing {
        fmt.printf("Checkpointing: every %ds\n", cfg.checkpoint_interval)
    }
    fmt.println("==========================================")

    if dry_run {
        fmt.println("")
        fmt.printf("[dry-run] Would launch %s with plan. No changes were made.\n", cfg.agent)
        return
    }

    if plan_file != "" {
        // Start auto-checkpointing before agent runs
        checkpoint_t: ^thread.Thread = nil
        if checkpointing {
            g_checkpoint_state = Checkpoint_State{
                sprite_name  = sprite_name,
                interval_sec = cfg.checkpoint_interval,
                stop         = false,
            }
            checkpoint_t = thread.create_and_start(checkpoint_thread_proc)
            fmt.printf("Auto-checkpointing every %ds\n", cfg.checkpoint_interval)
        }

        fmt.printf("Launching %s with plan...\n", cfg.agent)

        if cfg.agent == "claude" {
            model_flag := ""
            if cfg.model != "" {
                model_flag = fmt.tprintf("--model %s ", cfg.model)
            }
            agent_cmd := fmt.tprintf(
                "cd /home/sprite && claude %s-p 'read plan.md and complete the plan please'",
                model_flag,
            )
            sx(sprite_name, agent_cmd, false)
        } else if cfg.agent == "opencode" {
            oc_model := cfg.model
            if oc_model == "" {
                oc_model = "opencode/big-pickle"
            }
            agent_cmd := fmt.tprintf(
                "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m %s 'read plan.md and complete the plan please'",
                oc_model,
            )
            sx(sprite_name, agent_cmd, false)
        }

        // Stop checkpointing
        if checkpoint_t != nil {
            sync.mutex_lock(&g_checkpoint_state.mutex)
            g_checkpoint_state.stop = true
            sync.mutex_unlock(&g_checkpoint_state.mutex)
            thread.join(checkpoint_t)
            thread.destroy(checkpoint_t)
        }

        // Final checkpoint
        fmt.println("Creating final checkpoint...")
        final_cmd := fmt.tprintf("sprite checkpoint create -s %s 2>/dev/null", sprite_name)
        ret := run_cmd(final_cmd)
        if ret == 0 {
            fmt.println("Final checkpoint saved.")
        } else {
            fmt.println("Final checkpoint failed (non-fatal).")
        }
    } else {
        fmt.println("Opening console...")
        console_cmd := fmt.tprintf("sprite console -s %s", sprite_name)
        run_cmd(console_cmd)
    }
}

launch_usage :: proc() {
    fmt.eprintln("Usage: sprite-tool launch [options] <sprite-name> [plan-file]")
    fmt.eprintln("")
    fmt.eprintln("Options:")
    fmt.eprintln("  --dry-run              Show what would happen without executing")
    fmt.eprintln("  --no-checkpoint        Disable auto-checkpointing")
    fmt.eprintln("  --upload <dir>         Upload a local directory to /home/sprite/<dirname>")
    fmt.eprintln("                         (repeatable: --upload ./data --upload ./tests)")
    fmt.eprintln("")
    fmt.eprintln("Environment variables:")
    fmt.eprintln("  ENV_FILE               Path to .env file (default: ./.env)")
    fmt.eprintln("  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)")
    fmt.eprintln("  AGENT                  \"opencode\" (default) or \"claude\"")
    fmt.eprintln("  CLAUDE_AUTH            \"subscription\" (default) or \"apikey\"")
    fmt.eprintln("  MODEL                  Model override")
    fmt.eprintln("  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)")
    fmt.eprintln("")
    fmt.eprintln("Model examples:")
    fmt.eprintln("  OpenCode: MODEL=opencode/big-pickle  (free, default)")
    fmt.eprintln("            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)")
    fmt.eprintln("            MODEL=openai/gpt-4o")
    fmt.eprintln("            MODEL=google/gemini-2.5-pro")
    fmt.eprintln("  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku")
    fmt.eprintln("")
    fmt.eprintln("Examples:")
    fmt.eprintln("  sprite-tool launch my-project plan.md")
    fmt.eprintln("  sprite-tool launch --upload ./data my-project plan.md")
    fmt.eprintln("  sprite-tool launch --upload ./data --upload ./tests my-project plan.md")
    fmt.eprintln("  sprite-tool launch --dry-run my-project plan.md")
}
