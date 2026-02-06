"""sprite-tool watch: Poll beads task for progress."""

from python import Python, PythonObject


def _usage() raises:
    print(
        "Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]\n"
        "\n"
        "Arguments:\n"
        "  sprite-name     Name of the sprite to watch\n"
        "  task-id         Beads task ID to track (default: auto-detect first open critical task)\n"
        "  poll-interval   Seconds between polls (default: 30)\n"
        "\n"
        "Examples:\n"
        "  sprite-tool watch ember-red-hawk\n"
        "  sprite-tool watch ember-red-hawk CRM-1\n"
        "  sprite-tool watch ember-red-hawk CRM-1 60"
    )
    var sys_mod = Python.import_module("sys")
    _ = sys_mod.exit(1)


def _sx(sprite_name: String, cmd: String) raises -> String:
    """Run a command inside a sprite, capturing output. Suppresses stderr."""
    var subprocess = Python.import_module("subprocess")

    var proc_args = Python.list()
    proc_args.append("sprite")
    proc_args.append("exec")
    proc_args.append("-s")
    proc_args.append(sprite_name)
    proc_args.append("bash")
    proc_args.append("-c")
    proc_args.append(cmd)

    var result = subprocess.run(proc_args, capture_output=True, text=True)
    var stdout = result.stdout
    if stdout is Python.none():
        return ""
    return str(stdout).strip()


def _sx_print(sprite_name: String, cmd: String) raises -> String:
    """Run a command inside a sprite, printing output directly."""
    var output = _sx(sprite_name, cmd)
    if len(output) > 0:
        print(output)
    return output


def run(args: PythonObject) raises:
    """Execute the watch subcommand."""
    var sys_mod = Python.import_module("sys")
    var time_mod = Python.import_module("time")

    if int(len(args)) < 1:
        _usage()

    var sprite_name = str(args[0])
    var task_id: String = ""
    if int(len(args)) > 1:
        task_id = str(args[1])
    var poll_interval: Int = 30
    if int(len(args)) > 2:
        try:
            poll_interval = int(str(args[2]))
        except:
            print("Error: invalid poll-interval '" + str(args[2]) + "' (must be integer)")
            _ = sys_mod.exit(1)

    # Auto-detect tracker task if not specified
    if len(task_id) == 0:
        print("Detecting tracker task...")
        task_id = _sx(
            sprite_name,
            "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'",
        )
        if len(task_id) == 0:
            print("No critical task found. Falling back to first open task...")
            task_id = _sx(
                sprite_name,
                "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'",
            )
        if len(task_id) == 0:
            print("ERROR: No beads tasks found on sprite '" + sprite_name + "'")
            print("Specify a task ID manually: sprite-tool watch " + sprite_name + " <task-id>")
            _ = sys_mod.exit(1)
        print("Tracking task: " + task_id)

    print("Watching sprite '" + sprite_name + "' task '" + task_id + "' (every " + str(poll_interval) + "s)")
    print("Press Ctrl+C to stop")
    print("")

    while True:
        # Clear screen using ANSI escape codes
        print("\x1b[2J\x1b[H", end="")
        var now = str(time_mod.strftime("%H:%M:%S"))
        print("=== sprite-watch: " + sprite_name + " / " + task_id + " === " + now + " ===")
        print("")

        # Show task status
        var output = _sx_print(
            sprite_name,
            "cd /home/sprite && bd show " + task_id + " 2>/dev/null",
        )
        if len(output) == 0:
            print("(could not read task)")
        print("")

        # Show recent comments
        print("--- Recent updates ---")
        output = _sx_print(
            sprite_name,
            "cd /home/sprite && bd comments " + task_id + " 2>/dev/null | tail -8",
        )
        if len(output) == 0:
            print("(no comments)")
        print("")

        # Check if done
        var status = _sx(
            sprite_name,
            "cd /home/sprite && bd show " + task_id + " 2>/dev/null | grep -i status",
        )
        if len(status) > 0:
            var status_lower = status.lower()
            if "closed" in status_lower or "done" in status_lower or "completed" in status_lower:
                print("==========================================")
                print("PROJECT COMPLETE")
                print("==========================================")
                break

        _ = time_mod.sleep(poll_interval)
