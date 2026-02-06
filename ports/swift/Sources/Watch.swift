import Foundation

/// Run the watch subcommand: poll beads task for progress.
func watchCommand(spriteName: String, taskId: String?, pollInterval: Int?) {
    let interval = pollInterval ?? 30
    let sx = SpriteExec(sprite: spriteName, dryRun: false)

    // Auto-detect task if not specified
    let resolvedTaskId: String
    if let id = taskId, !id.isEmpty {
        resolvedTaskId = id
    } else {
        resolvedTaskId = autoDetectTask(sx: sx, spriteName: spriteName)
    }

    print("Watching sprite '\(spriteName)' task '\(resolvedTaskId)' (every \(interval)s)")
    print("Press Ctrl+C to stop")
    print("")

    while true {
        // Clear screen using ANSI escape codes
        print("\u{1b}[2J\u{1b}[H", terminator: "")
        fflush(stdout)

        // Get current time
        let timeStr = getTimestamp()
        print("=== sprite-watch: \(spriteName) / \(resolvedTaskId) === \(timeStr) ===")
        print("")

        // Show task status
        let statusOutput = sx.sxQuiet("cd /home/sprite && bd show \(resolvedTaskId) 2>/dev/null")
        if statusOutput.isEmpty {
            print("(could not read task)")
        } else {
            print(statusOutput)
        }
        print("")

        // Show recent comments
        print("--- Recent updates ---")
        let commentsOutput = sx.sxQuiet("cd /home/sprite && bd comments \(resolvedTaskId) 2>/dev/null | tail -8")
        if commentsOutput.isEmpty {
            print("(no comments)")
        } else {
            print(commentsOutput)
        }
        print("")

        // Check if done
        let statusLine = sx.sxQuiet("cd /home/sprite && bd show \(resolvedTaskId) 2>/dev/null | grep -i status")
        let statusLower = statusLine.lowercased()
        if statusLower.contains("closed")
            || statusLower.contains("done")
            || statusLower.contains("completed")
        {
            print("==========================================")
            print("PROJECT COMPLETE")
            print("==========================================")
            break
        }

        Thread.sleep(forTimeInterval: Double(interval))
    }
}

/// Auto-detect the beads task ID by querying the sprite.
private func autoDetectTask(sx: SpriteExec, spriteName: String) -> String {
    print("Detecting tracker task...")

    // Try critical priority first
    let criticalTask = sx.sxQuiet(
        "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'"
    )
    if !criticalTask.isEmpty {
        print("Tracking task: \(criticalTask)")
        return criticalTask
    }

    // Fallback to first open task
    print("No critical task found. Falling back to first open task...")
    let firstTask = sx.sxQuiet(
        "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'"
    )
    if !firstTask.isEmpty {
        print("Tracking task: \(firstTask)")
        return firstTask
    }

    fputs("ERROR: No beads tasks found on sprite '\(spriteName)'\n", stderr)
    fputs("Specify a task ID manually: sprite-tool watch \(spriteName) <task-id>\n", stderr)
    exit(1)
}
