"""sprite-tool watch: Poll beads task for progress."""

import subprocess
import sys
import time


def _usage():
    print("""\
Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

Arguments:
  sprite-name     Name of the sprite to watch
  task-id         Beads task ID to track (default: auto-detect first open critical task)
  poll-interval   Seconds between polls (default: 30)

Examples:
  sprite-tool watch ember-red-hawk
  sprite-tool watch ember-red-hawk CRM-1
  sprite-tool watch ember-red-hawk CRM-1 60""")
    sys.exit(1)


def _sx(sprite_name, cmd):
    """Run a command inside a sprite, capturing output. Suppresses stderr."""
    result = subprocess.run(
        ["sprite", "exec", "-s", sprite_name, "bash", "-c", cmd],
        capture_output=True,
        text=True,
    )
    return result.stdout.strip() if result.stdout else ""


def _sx_print(sprite_name, cmd):
    """Run a command inside a sprite, printing output directly."""
    result = subprocess.run(
        ["sprite", "exec", "-s", sprite_name, "bash", "-c", cmd],
        capture_output=True,
        text=True,
    )
    output = result.stdout.strip() if result.stdout else ""
    if output:
        print(output)
    return output


def run(args):
    """Execute the watch subcommand."""
    if len(args) < 1:
        _usage()

    sprite_name = args[0]
    task_id = args[1] if len(args) > 1 else ""
    poll_interval = 30
    if len(args) > 2:
        try:
            poll_interval = int(args[2])
        except ValueError:
            print(f"Error: invalid poll-interval '{args[2]}' (must be integer)")
            sys.exit(1)

    # Auto-detect tracker task if not specified
    if not task_id:
        print("Detecting tracker task...")
        task_id = _sx(
            sprite_name,
            "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'",
        )
        if not task_id:
            print("No critical task found. Falling back to first open task...")
            task_id = _sx(
                sprite_name,
                "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'",
            )
        if not task_id:
            print(f"ERROR: No beads tasks found on sprite '{sprite_name}'")
            print(
                f"Specify a task ID manually: sprite-tool watch {sprite_name} <task-id>"
            )
            sys.exit(1)
        print(f"Tracking task: {task_id}")

    print(f"Watching sprite '{sprite_name}' task '{task_id}' (every {poll_interval}s)")
    print("Press Ctrl+C to stop")
    print()

    try:
        while True:
            # Clear screen using ANSI escape codes
            print("\033[2J\033[H", end="", flush=True)
            now = time.strftime("%H:%M:%S")
            print(f"=== sprite-watch: {sprite_name} / {task_id} === {now} ===")
            print()

            # Show task status
            output = _sx_print(
                sprite_name,
                f"cd /home/sprite && bd show {task_id} 2>/dev/null",
            )
            if not output:
                print("(could not read task)")
            print()

            # Show recent comments
            print("--- Recent updates ---")
            output = _sx_print(
                sprite_name,
                f"cd /home/sprite && bd comments {task_id} 2>/dev/null | tail -8",
            )
            if not output:
                print("(no comments)")
            print()

            # Check if done
            status = _sx(
                sprite_name,
                f"cd /home/sprite && bd show {task_id} 2>/dev/null | grep -i status",
            )
            if status:
                status_lower = status.lower()
                if any(
                    word in status_lower for word in ("closed", "done", "completed")
                ):
                    print("==========================================")
                    print("PROJECT COMPLETE")
                    print("==========================================")
                    break

            time.sleep(poll_interval)

    except KeyboardInterrupt:
        print()
        print("Stopped.")
