-- watch.lua: sprite-tool watch — Poll a sprite's beads tracker task for progress.

local sprite = require("sprite")

local M = {}

local function usage()
    print([[Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

Arguments:
  sprite-name     Name of the sprite to watch
  task-id         Beads task ID to track (default: auto-detect first open critical task)
  poll-interval   Seconds between polls (default: 30)

Examples:
  sprite-tool watch ember-red-hawk
  sprite-tool watch ember-red-hawk CRM-1
  sprite-tool watch ember-red-hawk CRM-1 60]])
    os.exit(1)
end

--- Main entry point for the watch subcommand.
function M.run(args)
    if #args < 1 then usage() end

    local sprite_name   = args[1]
    local task_id       = args[2] or ""
    local poll_interval = tonumber(args[3]) or 30

    -- Auto-detect tracker task if not specified
    if task_id == "" then
        print("Detecting tracker task...")
        task_id = sprite.sx_capture(sprite_name,
            [[cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}']])

        if not task_id or task_id == "" then
            print("No critical task found. Falling back to first open task...")
            task_id = sprite.sx_capture(sprite_name,
                [[cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}']])
        end

        if not task_id or task_id == "" then
            io.stderr:write("ERROR: No beads tasks found on sprite '" .. sprite_name .. "'\n")
            io.stderr:write("Specify a task ID manually: sprite-tool watch " .. sprite_name .. " <task-id>\n")
            os.exit(1)
        end

        print("Tracking task: " .. task_id)
    end

    print("Watching sprite '" .. sprite_name .. "' task '" .. task_id .. "' (every " .. poll_interval .. "s)")
    print("Press Ctrl+C to stop")
    print("")

    while true do
        -- Clear screen (ANSI escape)
        io.write("\27[2J\27[H")

        -- Get current time
        local ts = os.date("%H:%M:%S")
        print("=== sprite-watch: " .. sprite_name .. " / " .. task_id .. " === " .. ts .. " ===")
        print("")

        -- Show task status
        local task_output = sprite.sx_capture(sprite_name,
            "cd /home/sprite && bd show " .. task_id .. " 2>/dev/null")
        if task_output and task_output ~= "" then
            print(task_output)
        else
            print("(could not read task)")
        end
        print("")

        -- Show recent comments
        print("--- Recent updates ---")
        local comments = sprite.sx_capture(sprite_name,
            "cd /home/sprite && bd comments " .. task_id .. " 2>/dev/null | tail -8")
        if comments and comments ~= "" then
            print(comments)
        else
            print("(no comments)")
        end
        print("")

        -- Check if done
        local status = sprite.sx_capture(sprite_name,
            "cd /home/sprite && bd show " .. task_id .. " 2>/dev/null | grep -i status")
        if status then
            local lower = status:lower()
            if lower:find("closed") or lower:find("done") or lower:find("completed") then
                print("==========================================")
                print("PROJECT COMPLETE")
                print("==========================================")
                break
            end
        end

        os.execute("sleep " .. poll_interval)
    end

    return 0
end

return M
