/**
 * Watch implements the sprite-watch subcommand.
 * Polls a sprite's beads tracker task for progress.
 *
 * Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]
 */
class Watch {
    public static function usage():Void {
        Sys.println("Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]");
        Sys.println("");
        Sys.println("Arguments:");
        Sys.println("  sprite-name     Name of the sprite to watch");
        Sys.println("  task-id         Beads task ID to track (default: auto-detect first open critical task)");
        Sys.println("  poll-interval   Seconds between polls (default: 30)");
        Sys.println("");
        Sys.println("Examples:");
        Sys.println("  sprite-tool watch ember-red-hawk");
        Sys.println("  sprite-tool watch ember-red-hawk CRM-1");
        Sys.println("  sprite-tool watch ember-red-hawk CRM-1 60");
    }

    public static function run(args:Array<String>):Void {
        if (args.length < 1) {
            usage();
            Sys.exit(1);
        }

        var spriteName = args[0];
        var taskId = if (args.length > 1) args[1] else "";
        var pollIntervalStr = if (args.length > 2) args[2] else "30";
        var pollInterval:Float = Std.parseFloat(pollIntervalStr);
        if (Math.isNaN(pollInterval) || pollInterval <= 0) {
            pollInterval = 30;
        }

        // Auto-detect tracker task if not specified
        if (taskId.length == 0) {
            Sys.println("Detecting tracker task...");
            taskId = StringTools.trim(
                Sprite.sxCapture(spriteName, "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'")
            );
            if (taskId.length == 0) {
                Sys.println("No critical task found. Falling back to first open task...");
                taskId = StringTools.trim(
                    Sprite.sxCapture(spriteName, "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'")
                );
            }
            if (taskId.length == 0) {
                Sys.println('ERROR: No beads tasks found on sprite \'$spriteName\'');
                Sys.println('Specify a task ID manually: sprite-tool watch $spriteName <task-id>');
                Sys.exit(1);
            }
            Sys.println('Tracking task: $taskId');
        }

        Sys.println('Watching sprite \'$spriteName\' task \'$taskId\' (every ${Std.string(Std.int(pollInterval))}s)');
        Sys.println("Press Ctrl+C to stop");
        Sys.println("");

        while (true) {
            // Clear screen
            Sys.print("\x1b[2J\x1b[H");

            // Get current time for display
            var now = Date.now();
            var timeStr = StringTools.lpad(Std.string(now.getHours()), "0", 2)
                + ":" + StringTools.lpad(Std.string(now.getMinutes()), "0", 2)
                + ":" + StringTools.lpad(Std.string(now.getSeconds()), "0", 2);
            Sys.println('=== sprite-watch: $spriteName / $taskId === $timeStr ===');
            Sys.println("");

            // Show task status
            var showRc = Sprite.sx(spriteName, 'cd /home/sprite && bd show $taskId 2>/dev/null', false);
            if (showRc != 0) {
                Sys.println("(could not read task)");
            }
            Sys.println("");

            // Show recent comments
            Sys.println("--- Recent updates ---");
            var commentsRc = Sprite.sx(spriteName, 'cd /home/sprite && bd comments $taskId 2>/dev/null | tail -8', false);
            if (commentsRc != 0) {
                Sys.println("(no comments)");
            }
            Sys.println("");

            // Check if done
            var status = Sprite.sxCapture(spriteName, 'cd /home/sprite && bd show $taskId 2>/dev/null | grep -i status');
            var statusLower = status.toLowerCase();
            if (statusLower.indexOf("closed") >= 0 || statusLower.indexOf("done") >= 0 || statusLower.indexOf("completed") >= 0) {
                Sys.println("==========================================");
                Sys.println("PROJECT COMPLETE");
                Sys.println("==========================================");
                break;
            }

            Sys.sleep(pollInterval);
        }
    }
}
