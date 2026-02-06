import sys.FileSystem;

/**
 * Push implements the sprite-push subcommand.
 * Pushes a local file or directory to a sprite.
 *
 * Usage: sprite-tool push <local-path> <remote-path> [sprite-name]
 */
class Push {
    public static function usage():Void {
        Sys.println("Usage: sprite-tool push <local-path> <remote-path> [sprite-name]");
        Sys.println("");
        Sys.println("Examples:");
        Sys.println("  sprite-tool push ./file.txt /home/sprite/file.txt");
        Sys.println("  sprite-tool push ./mydir /home/sprite/mydir");
        Sys.println("  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk");
    }

    public static function run(args:Array<String>):Void {
        if (args.length < 2) {
            usage();
            Sys.exit(1);
        }

        var localPath = args[0];
        var remotePath = args[1];
        var spriteName = if (args.length > 2) args[2] else "";

        if (!FileSystem.exists(localPath)) {
            Sys.stderr().writeString('Error: $localPath does not exist\n');
            Sys.exit(1);
        }

        if (FileSystem.isDirectory(localPath)) {
            Sys.println('Pushing directory: $localPath -> $remotePath');
            Sprite.pushDirToSprite(spriteName, localPath, remotePath);
        } else {
            Sys.println('Pushing file: $localPath -> $remotePath');
            Sprite.pushFileToSprite(spriteName, localPath, remotePath);
        }

        Sys.println("Done.");
    }
}
