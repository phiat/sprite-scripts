-- launch.lua: sprite-tool launch — Create and configure a sprite with coding agent, git, beads.

local config = require("config")
local sprite = require("sprite")

local M = {}

local function usage()
    print([[Usage: sprite-tool launch [options] <sprite-name> [plan-file]

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
  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md]])
    os.exit(1)
end

--- Check if a file exists.
local function file_exists(path)
    local fh = io.open(path, "r")
    if fh then
        fh:close()
        return true
    end
    return false
end

--- Check if a directory exists.
local function dir_exists(path)
    -- Use test -d via shell; os.execute returns true on success in Lua 5.2+
    local ok = os.execute("test -d " .. config.shell_quote(path))
    return ok == true or ok == 0
end

--- Start background checkpointing using a shell background process.
-- Returns a description string. The background process runs until the
-- parent shell pipeline ends or we explicitly kill it.
local function start_checkpointing(sprite_name, interval)
    local sq = config.shell_quote(sprite_name)
    -- Launch a background subshell that loops: sleep, checkpoint, repeat
    local bg_cmd = string.format(
        [[(while true; do sleep %d; echo "[checkpoint] Creating checkpoint at $(date '+%%H:%%M:%%S')..."; sprite checkpoint create -s %s 2>/dev/null && echo "[checkpoint] Done." || echo "[checkpoint] Failed (non-fatal)."; done) &]],
        interval, sq
    )
    os.execute(bg_cmd)
    print("Auto-checkpointing every " .. interval .. "s (background process)")
end

--- Stop background checkpointing by killing any matching background sleep/checkpoint loops.
local function stop_checkpointing(sprite_name)
    -- Best-effort kill of the background checkpoint loop
    local sq = config.shell_quote(sprite_name)
    os.execute("pkill -f 'sprite checkpoint create -s " .. sprite_name .. "' 2>/dev/null || true")
end

--- Main entry point for the launch subcommand.
function M.run(args)
    -- Load config (parses .env, reads env vars)
    local cfg = config.load()

    -- Parse flags
    local dry_run = false
    local checkpointing = true
    local upload_dirs = {}

    local i = 1
    while i <= #args and args[i]:sub(1, 2) == "--" do
        local flag = args[i]
        if flag == "--dry-run" then
            dry_run = true
            i = i + 1
        elseif flag == "--no-checkpoint" then
            checkpointing = false
            i = i + 1
        elseif flag == "--upload" then
            i = i + 1
            if i > #args then usage() end
            upload_dirs[#upload_dirs + 1] = args[i]
            i = i + 1
        elseif flag == "--help" or flag == "-h" then
            usage()
        else
            io.stderr:write("Unknown option: " .. flag .. "\n")
            usage()
        end
    end

    if i > #args then usage() end

    local sprite_name = args[i]
    i = i + 1
    local plan_file = args[i] -- may be nil

    -- Store into config
    cfg.dry_run = dry_run
    cfg.checkpointing = checkpointing
    cfg.upload_dirs = upload_dirs

    -- ============================================================
    -- 1. Check/install sprite CLI
    -- ============================================================
    local has_sprite = os.execute("command -v sprite >/dev/null 2>&1")
    if not (has_sprite == true or has_sprite == 0) then
        if dry_run then
            print("  [dry-run] Would install sprite CLI")
        else
            print("Installing sprite CLI...")
            os.execute("curl -fsSL https://sprites.dev/install.sh | sh")
            -- Note: PATH update only affects child processes in Lua
        end
    end

    -- ============================================================
    -- 2. Auth sprite (non-interactive if token provided)
    -- ============================================================
    if cfg.sprite_token ~= "" then
        print("Authenticating sprite with token...")
        if not dry_run then
            os.execute("sprite auth setup --token " .. config.shell_quote(cfg.sprite_token))
        end
    else
        print("No SPRITE_TOKEN set. Running interactive login...")
        if not dry_run then
            os.execute("sprite login")
        end
    end

    -- ============================================================
    -- 3. Create sprite (or use existing)
    -- ============================================================
    if dry_run then
        print("  [dry-run] Would create or reuse sprite '" .. sprite_name .. "'")
    else
        local handle = io.popen("sprite ls 2>/dev/null", "r")
        local ls_output = ""
        if handle then
            ls_output = handle:read("*a") or ""
            handle:close()
        end

        -- Check if sprite name appears as a whole word in ls output
        if ls_output:find(sprite_name, 1, true) then
            print("Sprite '" .. sprite_name .. "' already exists, using it.")
        else
            print("Creating sprite: " .. sprite_name)
            os.execute("sprite create -skip-console " .. config.shell_quote(sprite_name))
        end
    end

    -- ============================================================
    -- 4. Push .env to sprite
    -- ============================================================
    if file_exists(cfg.env_file) then
        print("Pushing " .. cfg.env_file .. "...")
        sprite.push_file(sprite_name, cfg.env_file, "/home/sprite/.env", dry_run)
    end

    -- ============================================================
    -- 5. Push plan file if provided
    -- ============================================================
    if plan_file and plan_file ~= "" and file_exists(plan_file) then
        print("Pushing " .. plan_file .. "...")
        sprite.push_file(sprite_name, plan_file, "/home/sprite/plan.md", dry_run)
    end

    -- ============================================================
    -- 6. Upload directories if provided
    -- ============================================================
    for _, dir in ipairs(upload_dirs) do
        if dir_exists(dir) then
            local dirname = sprite.basename(dir)
            print("Uploading directory: " .. dir .. " -> /home/sprite/" .. dirname)
            sprite.push_dir(sprite_name, dir, "/home/sprite/" .. dirname, dry_run)
        else
            print("WARNING: --upload dir '" .. dir .. "' not found, skipping.")
        end
    end

    -- ============================================================
    -- 7. Setup git + beads
    -- ============================================================
    print("Initializing git...")
    sprite.sx(sprite_name, "cd /home/sprite && git init -b main 2>/dev/null || true", dry_run)

    print("Installing beads...")
    sprite.sx(sprite_name, "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash", dry_run)

    -- ============================================================
    -- 8. Install and auth coding agent
    -- ============================================================
    local agent = cfg.agent
    local model = cfg.model

    if agent == "claude" then
        print("Setting up claude...")
        sprite.sx(sprite_name, "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code", dry_run)

        if cfg.claude_auth == "subscription" then
            local home = os.getenv("HOME") or ""
            local creds = home .. "/.claude/.credentials.json"
            if file_exists(creds) then
                print("Copying claude subscription credentials...")
                sprite.push_file(sprite_name, creds, "/home/sprite/.claude/.credentials.json", dry_run)
                sprite.sx(sprite_name, "chmod 600 ~/.claude/.credentials.json", dry_run)
            else
                io.stderr:write("ERROR: ~/.claude/.credentials.json not found\n")
                io.stderr:write("Run 'claude' locally first to authenticate, then re-run this script.\n")
                os.exit(1)
            end
        elseif cfg.claude_auth == "apikey" and cfg.anthropic_api_key ~= "" then
            print("Setting ANTHROPIC_API_KEY in sprite...")
            local api_key = cfg.anthropic_api_key
            sprite.sx(sprite_name,
                "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"" .. api_key .. "\"' >> ~/.bashrc",
                dry_run)
        else
            io.stderr:write("ERROR: No valid claude auth configured\n")
            io.stderr:write("Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY\n")
            os.exit(1)
        end

    elseif agent == "opencode" then
        print("Setting up opencode...")
        sprite.sx(sprite_name, "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash", dry_run)
        sprite.sx(sprite_name,
            [[grep -q 'source.*\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc]],
            dry_run)
    else
        io.stderr:write("ERROR: Unknown AGENT '" .. agent .. "'. Use 'claude' or 'opencode'.\n")
        os.exit(1)
    end

    -- ============================================================
    -- 9. Launch agent with plan (or open console)
    -- ============================================================
    print("")
    print("==========================================")
    print("Sprite '" .. sprite_name .. "' is ready!")
    local model_info = ""
    if model ~= "" then
        model_info = " (model: " .. model .. ")"
    end
    print("Agent: " .. agent .. model_info)
    if checkpointing then
        print("Checkpointing: every " .. cfg.checkpoint_interval .. "s")
    end
    print("==========================================")

    if dry_run then
        print("")
        print("[dry-run] Would launch " .. agent .. " with plan. No changes were made.")
        return 0
    end

    if plan_file and plan_file ~= "" then
        -- Start auto-checkpointing before agent runs
        if checkpointing then
            start_checkpointing(sprite_name, cfg.checkpoint_interval)
        end

        print("Launching " .. agent .. " with plan...")

        if agent == "claude" then
            local model_flag = ""
            if model ~= "" then
                model_flag = "--model " .. model .. " "
            end
            sprite.sx(sprite_name,
                "cd /home/sprite && claude " .. model_flag .. "-p 'read plan.md and complete the plan please'")

        elseif agent == "opencode" then
            local oc_model = model ~= "" and model or "opencode/big-pickle"
            sprite.sx(sprite_name,
                "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m " .. oc_model .. " 'read plan.md and complete the plan please'")
        end

        -- Final checkpoint after agent completes
        if checkpointing then
            stop_checkpointing(sprite_name)
        end
        print("Creating final checkpoint...")
        local ok = os.execute("sprite checkpoint create -s " .. config.shell_quote(sprite_name) .. " 2>/dev/null")
        if ok == true or ok == 0 then
            print("Final checkpoint saved.")
        else
            print("Final checkpoint failed (non-fatal).")
        end
    else
        print("Opening console...")
        os.execute("sprite console -s " .. config.shell_quote(sprite_name))
    end

    return 0
end

return M
