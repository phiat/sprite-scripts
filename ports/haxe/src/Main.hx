/**
 * Main is the entry point for sprite-tool.
 * Dispatches to subcommands: launch, push, pull, watch.
 *
 * Usage: sprite-tool <command> [args...]
 */
class Main {
    static function usage():Void {
        Sys.println("Usage: sprite-tool <command> [args...]");
        Sys.println("");
        Sys.println("Commands:");
        Sys.println("  launch    Create and configure a sprite with coding agent");
        Sys.println("  push      Push local file or directory to a sprite");
        Sys.println("  pull      Pull file or directory from a sprite");
        Sys.println("  watch     Watch a sprite task for progress");
        Sys.println("");
        Sys.println("Run 'sprite-tool <command> --help' for command-specific help.");
    }

    static function main():Void {
        var args = Sys.args();

        if (args.length < 1) {
            usage();
            Sys.exit(1);
        }

        var command = args[0];
        var subArgs = args.slice(1);

        switch (command) {
            case "launch":
                Launch.run(subArgs);
            case "push":
                Push.run(subArgs);
            case "pull":
                Pull.run(subArgs);
            case "watch":
                Watch.run(subArgs);
            case "--help", "-h":
                usage();
                Sys.exit(0);
            default:
                Sys.println('Unknown command: $command');
                Sys.println("");
                usage();
                Sys.exit(1);
        }
    }
}
