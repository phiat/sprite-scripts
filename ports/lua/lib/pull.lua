-- pull.lua: sprite-tool pull — Pull file or directory from a sprite.

local config = require("config")
local sprite = require("sprite")

local M = {}

local function usage()
    print([[Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

Examples:
  sprite-tool pull /home/sprite/file.txt ./file.txt
  sprite-tool pull /home/sprite/mydir ./mydir
  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk]])
    os.exit(1)
end

--- Main entry point for the pull subcommand.
function M.run(args)
    if #args < 2 then usage() end

    local remote_path = args[1]
    local local_path  = args[2]
    local sprite_name = args[3] or ""

    local sa = sprite.sprite_args(sprite_name)

    -- Check if remote path is a directory or file
    local check_cmd = "sprite exec " .. sa .. " bash -c "
                    .. config.shell_quote("[ -d '" .. remote_path .. "' ] && echo dir || echo file")
                    .. " 2>/dev/null"
    local handle = io.popen(check_cmd, "r")
    local is_dir = "file"
    if handle then
        is_dir = handle:read("*a") or "file"
        handle:close()
        is_dir = is_dir:match("^%s*(.-)%s*$") or "file"
    end

    if is_dir == "dir" then
        print("Pulling directory: " .. remote_path .. " -> " .. local_path)
        os.execute("mkdir -p " .. config.shell_quote(local_path))

        local pull_cmd = "sprite exec " .. sa .. " tar czf - -C "
                       .. config.shell_quote(remote_path) .. " ."
                       .. " | tar xzf - -C " .. config.shell_quote(local_path)
        os.execute(pull_cmd)
    else
        print("Pulling file: " .. remote_path .. " -> " .. local_path)
        local local_dir = sprite.dirname(local_path)
        if local_dir ~= "." and local_dir ~= "" then
            os.execute("mkdir -p " .. config.shell_quote(local_dir))
        end

        local cat_cmd = "sprite exec " .. sa .. " cat " .. config.shell_quote(remote_path)
        local cat_handle = io.popen(cat_cmd, "r")
        local content = ""
        if cat_handle then
            content = cat_handle:read("*a") or ""
            cat_handle:close()
        end

        local fh = io.open(local_path, "wb")
        if not fh then
            io.stderr:write("Error: cannot write to " .. local_path .. "\n")
            os.exit(1)
        end
        fh:write(content)
        fh:close()
    end

    print("Done.")
    return 0
end

return M
