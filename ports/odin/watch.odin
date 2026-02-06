package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:time"

cmd_watch :: proc(args: []string) {
    if len(args) < 1 {
        watch_usage()
        os.exit(1)
    }

    // Check for help flag
    for arg in args {
        if arg == "--help" || arg == "-h" {
            watch_usage()
            os.exit(0)
        }
    }

    sprite_name := args[0]
    task_id := ""
    if len(args) >= 2 {
        task_id = args[1]
    }
    poll_interval := 5
    if len(args) >= 3 {
        val, ok := strconv.parse_int(args[2])
        if ok {
            poll_interval = val
        }
    }

    // Auto-detect tracker task if not specified
    if task_id == "" {
        fmt.println("Detecting tracker task...")

        // Try critical tasks first
        critical_output := sx_capture(
            sprite_name,
            "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'",
        )
        critical_trimmed := strings.trim_space(critical_output)
        if critical_trimmed != "" {
            task_id = critical_trimmed
        }

        // Fallback to first open task
        if task_id == "" {
            fmt.println("No critical task found. Falling back to first open task...")
            open_output := sx_capture(
                sprite_name,
                "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'",
            )
            open_trimmed := strings.trim_space(open_output)
            if open_trimmed != "" {
                task_id = open_trimmed
            }
        }

        if task_id == "" {
            fmt.eprintf("ERROR: No beads tasks found on sprite '%s'\n", sprite_name)
            fmt.eprintf("Specify a task ID manually: sprite-tool watch %s <task-id>\n", sprite_name)
            os.exit(1)
        }

        fmt.printf("Tracking task: %s\n", task_id)
    }

    fmt.printf("Watching sprite '%s' task '%s' (every %ds)\n", sprite_name, task_id, poll_interval)
    fmt.println("Press Ctrl+C to stop")
    fmt.println("")

    for {
        // Clear screen (ANSI escape)
        fmt.print("\x1b[2J\x1b[H")
        fmt.printf("=== sprite-watch: %s / %s ===\n\n", sprite_name, task_id)

        // Show task status
        show_cmd := fmt.tprintf("cd /home/sprite && bd show %s 2>/dev/null", task_id)
        show_output := sx_capture(sprite_name, show_cmd)
        if show_output != "" {
            fmt.printf("%s\n\n", show_output)
        } else {
            fmt.println("(could not read task)\n")
        }

        // Show recent comments
        fmt.println("--- Recent updates ---")
        comments_cmd := fmt.tprintf("cd /home/sprite && bd comments %s 2>/dev/null | tail -8", task_id)
        comments_output := sx_capture(sprite_name, comments_cmd)
        if comments_output != "" {
            fmt.printf("%s\n\n", comments_output)
        } else {
            fmt.println("(no comments)\n")
        }

        // Check if done
        status_cmd := fmt.tprintf("cd /home/sprite && bd show %s 2>/dev/null | grep -i status", task_id)
        status_output := sx_capture(sprite_name, status_cmd)
        if status_output != "" {
            status_lower := strings.to_lower(status_output)
            if strings.contains(status_lower, "closed") ||
               strings.contains(status_lower, "done") ||
               strings.contains(status_lower, "completed") {
                fmt.println("==========================================")
                fmt.println("PROJECT COMPLETE")
                fmt.println("==========================================")
                break
            }
        }

        time.sleep(time.Duration(poll_interval) * time.Second)
    }
}

watch_usage :: proc() {
    fmt.eprintln("Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]")
    fmt.eprintln("")
    fmt.eprintln("Arguments:")
    fmt.eprintln("  sprite-name     Name of the sprite to watch")
    fmt.eprintln("  task-id         Beads task ID to track (default: auto-detect first open critical task)")
    fmt.eprintln("  poll-interval   Seconds between polls (default: 30)")
    fmt.eprintln("")
    fmt.eprintln("Examples:")
    fmt.eprintln("  sprite-tool watch ember-red-hawk")
    fmt.eprintln("  sprite-tool watch ember-red-hawk CRM-1")
    fmt.eprintln("  sprite-tool watch ember-red-hawk CRM-1 60")
}
