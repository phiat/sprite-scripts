import std.stdio : writeln, writefln, stderr;
import std.string : toLower;

import launch;
import push;
import pull;
import watch;

int main(string[] args)
{
    // args[0] is the program name; subcommand starts at args[1]
    if (args.length < 2)
    {
        writeln("Usage: sprite-tool <command> [args...]");
        writeln("");
        writeln("Commands:");
        writeln("  launch    Create and configure a sprite with coding agent");
        writeln("  push      Push local file or directory to a sprite");
        writeln("  pull      Pull file or directory from a sprite");
        writeln("  watch     Poll a sprite's beads tracker task for progress");
        return 1;
    }

    string command = args[1].toLower();
    string[] rest = args[2 .. $];

    switch (command)
    {
    case "launch":
        launch.run(rest);
        return 0;
    case "push":
        push.run(rest);
        return 0;
    case "pull":
        pull.run(rest);
        return 0;
    case "watch":
        watch.run(rest);
        return 0;
    default:
        stderr.writefln("Unknown command: %s", command);
        stderr.writeln("Available commands: launch, push, pull, watch");
        return 1;
    }
}
