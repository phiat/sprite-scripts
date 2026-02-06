-- sprite.lua: Sprite CLI execution helpers — sx, push_file, push_dir.

local config = require("config")

local M = {}

local shell_quote = config.shell_quote

--- Build the sprite args fragment (e.g. "-s 'my-sprite'" or "").
local function sprite_args(sprite_name)
    if sprite_name and sprite_name ~= "" then
        return "-s " .. shell_quote(sprite_name)
    end
    return ""
end
M.sprite_args = sprite_args

--- Run a command inside the sprite via bash.
-- Returns the exit code (0 = success).
function M.sx(sprite_name, cmd, dry_run)
    if dry_run then
        print('  [dry-run] sprite exec -s ' .. sprite_name .. ' bash -c "' .. cmd .. '"')
        return 0
    end

    local escaped = shell_quote(cmd)
    local full_cmd = "sprite exec -s " .. shell_quote(sprite_name) .. " bash -c " .. escaped
    local ok, exit_type, code = os.execute(full_cmd)
    if ok then return 0 end
    return code or 1
end

--- Run a command inside the sprite and capture stdout.
-- Returns the captured output string (trimmed).
function M.sx_capture(sprite_name, cmd, dry_run)
    if dry_run then
        print('  [dry-run] sprite exec -s ' .. sprite_name .. ' bash -c "' .. cmd .. '"')
        return ""
    end

    local escaped = shell_quote(cmd)
    local full_cmd = "sprite exec -s " .. shell_quote(sprite_name) .. " bash -c " .. escaped .. " 2>/dev/null"
    local handle = io.popen(full_cmd, "r")
    if not handle then return "" end
    local output = handle:read("*a")
    handle:close()

    -- Trim trailing whitespace/newlines
    if output then
        output = output:match("^(.-)%s*$") or ""
    else
        output = ""
    end

    return output
end

--- Get the directory part of a path (everything up to and including the last /).
local function dirname(path)
    local dir = path:match("^(.*)/[^/]*$")
    if dir and dir ~= "" then return dir end
    if path:sub(1, 1) == "/" then return "/" end
    return "."
end
M.dirname = dirname

--- Get the base name of a path (the last component).
local function basename(path)
    return path:match("([^/]+)$") or path
end
M.basename = basename

--- Push a local file to the sprite.
function M.push_file(sprite_name, src, dest, dry_run)
    if dry_run then
        print("  [dry-run] push " .. src .. " -> sprite:" .. dest)
        return 0
    end

    local dest_dir = dirname(dest)
    local sa = sprite_args(sprite_name)

    -- Create remote directory
    local mkdir_cmd = "sprite exec " .. sa .. " bash -c " .. shell_quote("mkdir -p '" .. dest_dir .. "'")
    os.execute(mkdir_cmd)

    -- Pipe file contents to sprite
    local cat_cmd = "sprite exec " .. sa .. " bash -c " .. shell_quote("cat > '" .. dest .. "'") .. " < " .. shell_quote(src)
    local ok, exit_type, code = os.execute(cat_cmd)
    if ok then return 0 end
    return code or 1
end

--- Push a local directory to the sprite (used by launch — preserves directory name).
function M.push_dir(sprite_name, src, dest, dry_run)
    if dry_run then
        print("  [dry-run] push dir " .. src .. " -> sprite:" .. dest)
        return 0
    end

    local sa = sprite_args(sprite_name)

    -- Create remote directory
    local mkdir_cmd = "sprite exec " .. sa .. " bash -c " .. shell_quote("mkdir -p '" .. dest .. "'")
    os.execute(mkdir_cmd)

    -- tar pipe: local tar -> sprite tar extract
    local src_dir = dirname(src)
    local src_base = basename(src)
    local dest_parent = dirname(dest)

    local tar_cmd = "tar czf - -C " .. shell_quote(src_dir) .. " " .. shell_quote(src_base)
                 .. " | sprite exec " .. sa .. " bash -c "
                 .. shell_quote("tar xzf - -C '" .. dest_parent .. "'")

    local ok, exit_type, code = os.execute(tar_cmd)
    if ok then return 0 end
    return code or 1
end

--- Push a local directory to the sprite with --strip-components=1.
-- Used by the standalone push subcommand where remote path IS the target dir.
function M.push_dir_strip(sprite_name, src, dest, dry_run)
    if dry_run then
        print("  [dry-run] push dir " .. src .. " -> sprite:" .. dest)
        return 0
    end

    local sa = sprite_args(sprite_name)
    local src_dir = dirname(src)
    local src_base = basename(src)

    local tar_cmd = "tar czf - -C " .. shell_quote(src_dir) .. " " .. shell_quote(src_base)
                 .. " | sprite exec " .. sa .. " bash -c "
                 .. shell_quote("mkdir -p '" .. dest .. "' && tar xzf - -C '" .. dest .. "' --strip-components=1")

    local ok, exit_type, code = os.execute(tar_cmd)
    if ok then return 0 end
    return code or 1
end

return M
