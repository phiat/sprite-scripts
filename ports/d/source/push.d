module push;

import std.file : exists, isDir;
import std.format : format;
import std.path : absolutePath, baseName, dirName;
import std.process : executeShell;
import std.stdio : writeln, writefln, stderr;

import sprite;

private void usage()
{
    writeln(
        "Usage: sprite-tool push <local-path> <remote-path> [sprite-name]\n"
        ~ "\n"
        ~ "Examples:\n"
        ~ "  sprite-tool push ./file.txt /home/sprite/file.txt\n"
        ~ "  sprite-tool push ./mydir /home/sprite/mydir\n"
        ~ "  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk"
    );

    import core.stdc.stdlib : exit;
    exit(1);
}

void run(string[] args)
{
    if (args.length < 2)
        usage();

    string localPath = args[0];
    string remotePath = args[1];
    string spriteName = args.length > 2 ? args[2] : "";

    string sArgs = spriteName.length > 0
        ? format!"-s %s"(spriteName) : "";

    if (!exists(localPath))
    {
        stderr.writefln("Error: %s does not exist", localPath);
        import core.stdc.stdlib : exit;
        exit(1);
    }

    if (isDir(localPath))
    {
        writefln("Pushing directory: %s -> %s", localPath, remotePath);

        string fullPath = absolutePath(localPath);
        string parent = dirName(fullPath);
        string base = baseName(fullPath);

        // tar czf - -C parent baseName | sprite exec [-s name] bash -c "mkdir -p DEST && tar xzf - -C DEST --strip-components=1"
        string shellCmd = format!"tar czf - -C %s %s | sprite exec %s bash -c %s"(
            shellQuote(parent),
            shellQuote(base),
            sArgs,
            shellQuote(format!"mkdir -p '%s' && tar xzf - -C '%s' --strip-components=1"(
                remotePath, remotePath)));
        auto result = executeShell(shellCmd);
        if (result.status != 0)
        {
            stderr.writeln("Error: push failed");
            import core.stdc.stdlib : exit;
            exit(1);
        }
    }
    else
    {
        writefln("Pushing file: %s -> %s", localPath, remotePath);

        string remoteDir = dirName(remotePath);

        // sprite exec [-s name] bash -c "mkdir -p DIR && cat > DEST" < localPath
        string shellCmd = format!"sprite exec %s bash -c %s < %s"(
            sArgs,
            shellQuote(format!"mkdir -p '%s' && cat > '%s'"(remoteDir, remotePath)),
            shellQuote(localPath));
        auto result = executeShell(shellCmd);
        if (result.status != 0)
        {
            stderr.writeln("Error: push failed");
            import core.stdc.stdlib : exit;
            exit(1);
        }
    }

    writeln("Done.");
}
