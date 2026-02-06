import sys.io.Process;
import sys.io.File;
import sys.FileSystem;

/**
 * Sprite provides utility functions for executing commands on sprites
 * and transferring files/directories.
 */
class Sprite {
    /**
     * Execute a bash command on a sprite via `sprite exec -s <name> bash -c "<cmd>"`.
     * In dry-run mode, prints the command instead of executing.
     * Returns the exit code (0 for dry-run).
     */
    public static function sx(spriteName:String, cmd:String, dryRun:Bool):Int {
        if (dryRun) {
            Sys.println('  [dry-run] sprite exec -s $spriteName bash -c "$cmd"');
            return 0;
        }
        return Sys.command("sprite", ["exec", "-s", spriteName, "bash", "-c", cmd]);
    }

    /**
     * Execute a bash command on a sprite and capture stdout.
     * Returns the trimmed stdout output.
     */
    public static function sxCapture(spriteName:String, cmd:String):String {
        var args = ["exec", "-s", spriteName, "bash", "-c", cmd];
        var proc = new Process("sprite", args);
        var output = proc.stdout.readAll().toString();
        proc.exitCode(); // wait for completion
        proc.close();
        return StringTools.trim(output);
    }

    /**
     * Execute a command on a sprite with optional -s flag.
     * If spriteName is empty, omits the -s flag.
     * Returns the exit code.
     */
    public static function execOnSprite(spriteName:String, args:Array<String>):Int {
        var fullArgs:Array<String> = ["exec"];
        if (spriteName.length > 0) {
            fullArgs.push("-s");
            fullArgs.push(spriteName);
        }
        for (a in args) {
            fullArgs.push(a);
        }
        return Sys.command("sprite", fullArgs);
    }

    /**
     * Execute a command on a sprite and capture stdout.
     * If spriteName is empty, omits the -s flag.
     */
    public static function execOnSpriteCapture(spriteName:String, args:Array<String>):String {
        var fullArgs:Array<String> = ["exec"];
        if (spriteName.length > 0) {
            fullArgs.push("-s");
            fullArgs.push(spriteName);
        }
        for (a in args) {
            fullArgs.push(a);
        }
        var proc = new Process("sprite", fullArgs);
        var output = proc.stdout.readAll().toString();
        proc.exitCode();
        proc.close();
        return StringTools.trim(output);
    }

    /**
     * Push a local file to a sprite at the given remote path.
     * Creates parent directories on the remote side.
     */
    public static function pushFile(spriteName:String, localPath:String, remotePath:String, dryRun:Bool):Void {
        if (dryRun) {
            Sys.println('  [dry-run] push $localPath -> sprite:$remotePath');
            return;
        }
        // Compute remote directory
        var remoteDir = dirname(remotePath);
        sx(spriteName, 'mkdir -p \'$remoteDir\'', false);

        // Pipe file content via stdin
        var args = ["exec", "-s", spriteName, "bash", "-c", 'cat > \'$remotePath\''];
        var proc = new Process("sprite", args);
        var content = File.getBytes(localPath);
        proc.stdin.writeBytes(content, 0, content.length);
        proc.stdin.close();
        proc.exitCode();
        proc.close();
    }

    /**
     * Push a local directory to a sprite at the given remote path.
     * Uses tar to pipe the directory contents.
     */
    public static function pushDir(spriteName:String, localPath:String, remotePath:String, dryRun:Bool):Void {
        if (dryRun) {
            Sys.println('  [dry-run] push dir $localPath -> sprite:$remotePath');
            return;
        }
        sx(spriteName, 'mkdir -p \'$remotePath\'', false);

        var parentDir = dirname(localPath);
        var baseName = basename(localPath);
        var remoteParent = dirname(remotePath);
        // Use shell pipeline: tar on local | sprite exec tar on remote
        Sys.command("bash", ["-c", 'tar czf - -C \'$parentDir\' \'$baseName\' | sprite exec -s \'$spriteName\' bash -c "tar xzf - -C \'$remoteParent\'"']);
    }

    /**
     * Push a local file to a sprite with optional -s flag (for push subcommand).
     * If spriteName is empty, omits the -s flag.
     */
    public static function pushFileToSprite(spriteName:String, localPath:String, remotePath:String):Void {
        var remoteDir = dirname(remotePath);
        // Create remote directory
        var mkdirArgs:Array<String> = ["exec"];
        if (spriteName.length > 0) {
            mkdirArgs.push("-s");
            mkdirArgs.push(spriteName);
        }
        mkdirArgs.push("bash");
        mkdirArgs.push("-c");
        mkdirArgs.push('mkdir -p \'$remoteDir\' && cat > \'$remotePath\'');

        var proc = new Process("sprite", mkdirArgs);
        var content = File.getBytes(localPath);
        proc.stdin.writeBytes(content, 0, content.length);
        proc.stdin.close();
        proc.exitCode();
        proc.close();
    }

    /**
     * Push a local directory to a sprite with optional -s flag (for push subcommand).
     * Uses tar with --strip-components=1 matching the bash push script.
     */
    public static function pushDirToSprite(spriteName:String, localPath:String, remotePath:String):Void {
        var parentDir = dirname(localPath);
        var baseName = basename(localPath);
        var spriteFlag = "";
        if (spriteName.length > 0) {
            spriteFlag = '-s \'$spriteName\'';
        }
        Sys.command("bash", [
            "-c",
            'tar czf - -C \'$parentDir\' \'$baseName\' | sprite exec $spriteFlag bash -c "mkdir -p \'$remotePath\' && tar xzf - -C \'$remotePath\' --strip-components=1"'
        ]);
    }

    /**
     * Pull a file from a sprite to local filesystem.
     */
    public static function pullFile(spriteName:String, remotePath:String, localPath:String):Void {
        var localDir = dirname(localPath);
        if (!FileSystem.exists(localDir)) {
            FileSystem.createDirectory(localDir);
        }
        var args:Array<String> = ["exec"];
        if (spriteName.length > 0) {
            args.push("-s");
            args.push(spriteName);
        }
        args.push("cat");
        args.push(remotePath);
        var proc = new Process("sprite", args);
        var content = proc.stdout.readAll();
        proc.exitCode();
        proc.close();
        File.saveBytes(localPath, content);
    }

    /**
     * Pull a directory from a sprite to local filesystem via tar.
     */
    public static function pullDir(spriteName:String, remotePath:String, localPath:String):Void {
        if (!FileSystem.exists(localPath)) {
            FileSystem.createDirectory(localPath);
        }
        var spriteFlag = "";
        if (spriteName.length > 0) {
            spriteFlag = '-s \'$spriteName\'';
        }
        Sys.command("bash", [
            "-c",
            'sprite exec $spriteFlag tar czf - -C \'$remotePath\' . | tar xzf - -C \'$localPath\''
        ]);
    }

    /**
     * Check if a remote path is a directory.
     */
    public static function isRemoteDir(spriteName:String, remotePath:String):Bool {
        var result = execOnSpriteCapture(spriteName, ["bash", "-c", '[ -d \'$remotePath\' ] && echo dir || echo file']);
        return StringTools.trim(result) == "dir";
    }

    /**
     * Get the directory component of a path (like dirname in bash).
     */
    public static function dirname(path:String):String {
        var idx = path.lastIndexOf("/");
        if (idx < 0) {
            return ".";
        }
        if (idx == 0) {
            return "/";
        }
        return path.substr(0, idx);
    }

    /**
     * Get the filename component of a path (like basename in bash).
     */
    public static function basename(path:String):String {
        // Remove trailing slashes
        var p = path;
        while (p.length > 1 && StringTools.endsWith(p, "/")) {
            p = p.substr(0, p.length - 1);
        }
        var idx = p.lastIndexOf("/");
        if (idx < 0) {
            return p;
        }
        return p.substr(idx + 1);
    }
}
