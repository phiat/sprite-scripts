module watch;

import std.algorithm : canFind;
import std.conv : to;
import std.datetime : Clock;
import std.format : format;
import std.process : executeShell;
import std.stdio : writeln, writefln, writef, stdout, stderr;
import std.string : strip, toLower;

import sprite;

import core.thread : Thread;
import core.time : dur;

private void usage()
{
    writeln(
        "Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]\n"
        ~ "\n"
        ~ "Arguments:\n"
        ~ "  sprite-name     Name of the sprite to watch\n"
        ~ "  task-id         Beads task ID to track (default: auto-detect first open critical task)\n"
        ~ "  poll-interval   Seconds between polls (default: 30)\n"
        ~ "\n"
        ~ "Examples:\n"
        ~ "  sprite-tool watch ember-red-hawk\n"
        ~ "  sprite-tool watch ember-red-hawk CRM-1\n"
        ~ "  sprite-tool watch ember-red-hawk CRM-1 60"
    );

    import core.stdc.stdlib : exit;
    exit(1);
}

/// Run command inside sprite, capture stdout (suppresses stderr).
private string sxCapture(string spriteName, string cmd)
{
    return sprite.sx(spriteName, cmd, false);
}

/// Run command inside sprite, print output, return it.
private string sxPrint(string spriteName, string cmd)
{
    string output = sxCapture(spriteName, cmd);
    if (output.length > 0)
        writeln(output);
    return output;
}

void run(string[] args)
{
    if (args.length < 1)
        usage();

    string spriteName = args[0];

    string taskId = args.length > 1 ? args[1] : "";

    int pollInterval = 30;
    if (args.length > 2)
    {
        try
        {
            pollInterval = to!int(args[2]);
        }
        catch (Exception e)
        {
            stderr.writefln("Error: invalid poll-interval '%s' (must be integer)", args[2]);
            import core.stdc.stdlib : exit;
            exit(1);
        }
    }

    // Auto-detect tracker task if not specified
    if (taskId.length == 0)
    {
        writeln("Detecting tracker task...");

        taskId = sxCapture(spriteName,
            "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'");

        if (taskId.length == 0)
        {
            writeln("No critical task found. Falling back to first open task...");
            taskId = sxCapture(spriteName,
                "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'");
        }

        if (taskId.length == 0)
        {
            stderr.writefln("ERROR: No beads tasks found on sprite '%s'", spriteName);
            stderr.writefln("Specify a task ID manually: sprite-tool watch %s <task-id>",
                spriteName);
            import core.stdc.stdlib : exit;
            exit(1);
        }

        writefln("Tracking task: %s", taskId);
    }

    writefln("Watching sprite '%s' task '%s' (every %ds)", spriteName, taskId, pollInterval);
    writeln("Press Ctrl+C to stop");
    writeln("");

    while (true)
    {
        // Clear screen using ANSI escape
        writef("\x1B[2J\x1B[H");
        stdout.flush();

        auto now = Clock.currTime();
        writefln("=== sprite-watch: %s / %s === %02d:%02d:%02d ===",
            spriteName, taskId, now.hour, now.minute, now.second);
        writeln("");

        // Show task status
        string taskOutput = sxPrint(spriteName,
            format!"cd /home/sprite && bd show %s 2>/dev/null"(taskId));
        if (taskOutput.length == 0)
            writeln("(could not read task)");

        writeln("");

        // Show recent comments
        writeln("--- Recent updates ---");
        string commentsOutput = sxPrint(spriteName,
            format!"cd /home/sprite && bd comments %s 2>/dev/null | tail -8"(taskId));
        if (commentsOutput.length == 0)
            writeln("(no comments)");

        writeln("");

        // Check if done
        string status = sxCapture(spriteName,
            format!"cd /home/sprite && bd show %s 2>/dev/null | grep -i status"(taskId));

        if (status.length > 0)
        {
            string statusLower = status.toLower();
            if (statusLower.canFind("closed")
                || statusLower.canFind("done")
                || statusLower.canFind("completed"))
            {
                writeln("==========================================");
                writeln("PROJECT COMPLETE");
                writeln("==========================================");
                break;
            }
        }

        Thread.sleep(dur!"seconds"(pollInterval));
    }
}
