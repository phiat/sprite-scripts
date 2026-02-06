using System;
using System.Threading;

namespace SpriteTool.Commands;

public static class WatchCommand
{
    public static int Execute(string spriteName, string? taskId, int pollInterval)
    {
        // Auto-detect tracker task if not specified
        if (string.IsNullOrEmpty(taskId))
        {
            Console.WriteLine("Detecting tracker task...");
            var (_, critOutput) = SpriteExec.SxCapture(spriteName,
                "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'");
            taskId = critOutput.Trim();

            if (string.IsNullOrEmpty(taskId))
            {
                Console.WriteLine("No critical task found. Falling back to first open task...");
                var (_, openOutput) = SpriteExec.SxCapture(spriteName,
                    "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'");
                taskId = openOutput.Trim();
            }

            if (string.IsNullOrEmpty(taskId))
            {
                Console.Error.WriteLine($"ERROR: No beads tasks found on sprite '{spriteName}'");
                Console.Error.WriteLine($"Specify a task ID manually: sprite-tool watch {spriteName} <task-id>");
                return 1;
            }

            Console.WriteLine($"Tracking task: {taskId}");
        }

        Console.WriteLine($"Watching sprite '{spriteName}' task '{taskId}' (every {pollInterval}s)");
        Console.WriteLine("Press Ctrl+C to stop");
        Console.WriteLine();

        while (true)
        {
            // Clear screen
            Console.Write("\x1b[2J\x1b[H");
            Console.WriteLine($"=== sprite-watch: {spriteName} / {taskId} === {DateTime.Now:HH:mm:ss} ===");
            Console.WriteLine();

            // Show task status
            var (showExit, showOutput) = SpriteExec.SxCapture(spriteName,
                $"cd /home/sprite && bd show {taskId} 2>/dev/null");
            if (showExit == 0 && !string.IsNullOrWhiteSpace(showOutput))
                Console.WriteLine(showOutput);
            else
                Console.WriteLine("(could not read task)");
            Console.WriteLine();

            // Show recent comments
            Console.WriteLine("--- Recent updates ---");
            var (commentsExit, commentsOutput) = SpriteExec.SxCapture(spriteName,
                $"cd /home/sprite && bd comments {taskId} 2>/dev/null | tail -8");
            if (commentsExit == 0 && !string.IsNullOrWhiteSpace(commentsOutput))
                Console.WriteLine(commentsOutput);
            else
                Console.WriteLine("(no comments)");
            Console.WriteLine();

            // Check if done
            var (_, statusOutput) = SpriteExec.SxCapture(spriteName,
                $"cd /home/sprite && bd show {taskId} 2>/dev/null | grep -i status");
            if (!string.IsNullOrEmpty(statusOutput))
            {
                var lower = statusOutput.ToLowerInvariant();
                if (lower.Contains("closed") || lower.Contains("done") || lower.Contains("completed"))
                {
                    Console.WriteLine("==========================================");
                    Console.WriteLine("PROJECT COMPLETE");
                    Console.WriteLine("==========================================");
                    break;
                }
            }

            Thread.Sleep(pollInterval * 1000);
        }

        return 0;
    }
}
