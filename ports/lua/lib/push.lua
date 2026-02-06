-- push.lua: sprite-tool push — Push local file or directory to a sprite.

local sprite = require("sprite")

local M = {}

local function usage()
    print([[Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

Examples:
  sprite-tool push ./file.txt /home/sprite/file.txt
  sprite-tool push ./mydir /home/sprite/mydir
  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk]])
    os.exit(1)
end

--- Check if a path exists (file or directory).
local function path_exists(path)
    local fh = io.open(path, "r")
    if fh then
        fh:close()
        return true
    end
    -- Also check if it's a directory
    local ok = os.execute("test -e " .. require("config").shell_quote(path))
    return ok == true or ok == 0
end

--- Check if a path is a directory.
local function is_dir(path)
    local ok = os.execute("test -d " .. require("config").shell_quote(path))
    return ok == true or ok == 0
end

--- Main entry point for the push subcommand.
function M.run(args)
    if #args < 2 then usage() end

    local local_path  = args[1]
    local remote_path = args[2]
    local sprite_name = args[3] or ""

    if not path_exists(local_path) then
        io.stderr:write("Error: " .. local_path .. " does not exist\n")
        os.exit(1)
    end

    if is_dir(local_path) then
        print("Pushing directory: " .. local_path .. " -> " .. remote_path)
        sprite.push_dir_strip(sprite_name, local_path, remote_path, false)
    else
        print("Pushing file: " .. local_path .. " -> " .. remote_path)
        sprite.push_file(sprite_name, local_path, remote_path, false)
    end

    print("Done.")
    return 0
end

return M
