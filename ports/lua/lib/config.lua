-- config.lua: Configuration loading — .env parsing and environment variable reading.

local M = {}

--- Shell-quote a string (for safe embedding in shell commands).
-- Wraps in single quotes, escaping embedded single quotes as '\''.
local function shell_quote(s)
    if s == nil then return "''" end
    return "'" .. s:gsub("'", "'\\''") .. "'"
end
M.shell_quote = shell_quote

--- Parse a .env file. Returns a table of key=value pairs.
-- Also sets values into the process environment (via os.execute) only for
-- keys not already present.  Because Lua cannot write to its own os.getenv
-- table at runtime, we keep a shadow table so later lookups see .env values.
function M.parse_env_file(path, shadow)
    shadow = shadow or {}
    local parsed = {}

    if not path then return parsed end

    local fh = io.open(path, "r")
    if not fh then return parsed end

    for line in fh:lines() do
        -- Trim leading/trailing whitespace
        line = line:match("^%s*(.-)%s*$")

        -- Skip blank lines and comments
        if line ~= "" and line:sub(1, 1) ~= "#" then
            -- Must contain =
            local eq = line:find("=")
            if eq then
                local key = line:sub(1, eq - 1):match("^%s*(.-)%s*$")
                local value = line:sub(eq + 1):match("^%s*(.-)%s*$")

                -- Strip matching quotes
                if #value >= 2 then
                    local first = value:sub(1, 1)
                    local last = value:sub(-1)
                    if first == last and (first == '"' or first == "'") then
                        value = value:sub(2, -2)
                    end
                end

                parsed[key] = value

                -- Set in shadow env (don't overwrite existing env or shadow)
                if not os.getenv(key) and not shadow[key] then
                    shadow[key] = value
                end
            end
        end
    end

    fh:close()
    return parsed
end

--- Get an environment variable, checking the shadow table first, then os.getenv.
function M.getenv(shadow, key)
    if shadow and shadow[key] then
        return shadow[key]
    end
    return os.getenv(key)
end

--- Load configuration from .env file and environment variables.
-- Returns a config table with all fields.
function M.load()
    local shadow = {}

    local env_file = os.getenv("ENV_FILE") or "./.env"

    -- Parse .env file (populates shadow for keys not already in os env)
    M.parse_env_file(env_file, shadow)

    -- Helper to look up from shadow or os env
    local function get(key)
        return M.getenv(shadow, key)
    end

    -- SPRITE_TOKEN with SPRITES_TOKEN fallback
    local sprite_token = get("SPRITE_TOKEN") or ""
    if sprite_token == "" then
        sprite_token = get("SPRITES_TOKEN") or ""
    end

    -- Agent
    local agent = get("AGENT") or "opencode"

    -- Claude auth
    local claude_auth = get("CLAUDE_AUTH") or "subscription"

    -- Anthropic API key
    local anthropic_api_key = get("ANTHROPIC_API_KEY") or ""

    -- Model
    local model = get("MODEL") or ""

    -- Checkpoint interval
    local interval_str = get("CHECKPOINT_INTERVAL") or "300"
    local checkpoint_interval = tonumber(interval_str)
    if not checkpoint_interval then
        io.stderr:write("Error: invalid CHECKPOINT_INTERVAL '" .. interval_str .. "' (must be integer)\n")
        os.exit(1)
    end

    return {
        sprite_token        = sprite_token,
        agent               = agent,
        claude_auth         = claude_auth,
        anthropic_api_key   = anthropic_api_key,
        model               = model,
        checkpoint_interval = checkpoint_interval,
        env_file            = env_file,
        env_shadow          = shadow,

        -- CLI flags (set by caller)
        dry_run       = false,
        checkpointing = true,
        upload_dirs   = {},
    }
end

return M
