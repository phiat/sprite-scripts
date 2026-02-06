import sys.FileSystem;

/**
 * Pull implements the sprite-pull subcommand.
 * Pulls a file or directory from a sprite to local filesystem.
 *
 * Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]
 */
class Pull {
    public static function usage():Void {
        Sys.println("Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]");
        Sys.println("");
        Sys.println("Examples:");
        Sys.println("  sprite-tool pull /home/sprite/file.txt ./file.txt");
        Sys.println("  sprite-tool pull /home/sprite/mydir ./mydir");
        Sys.println("  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk");
    }

    public static function run(args:Array<String>):Void {
        if (args.length < 2) {
            usage();
            Sys.exit(1);
        }

        var remotePath = args[0];
        var localPath = args[1];
        var spriteName = if (args.length > 2) args[2] else "";

        // Check if remote path is a directory
        var isDir = Sprite.isRemoteDir(spriteName, remotePath);

        if (isDir) {
            Sys.println('Pulling directory: $remotePath -> $localPath');
            Sprite.pullDir(spriteName, remotePath, localPath);
        } else {
            Sys.println('Pulling file: $remotePath -> $localPath');
            Sprite.pullFile(spriteName, remotePath, localPath);
        }

        Sys.println("Done.");
    }
}
