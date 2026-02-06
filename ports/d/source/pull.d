module pull;

import std.file : exists, mkdirRecurse;
import std.format : format;
import std.path : dirName;
import std.process : executeShell;
import std.stdio : writeln, writefln, stderr, File;
import std.string : strip;

import sprite;

private void usage()
{
    writeln(
        "Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]\n"
        ~ "\n"
        ~ "Examples:\n"
        ~ "  sprite-tool pull /home/sprite/file.txt ./file.txt\n"
        ~ "  sprite-tool pull /home/sprite/mydir ./mydir\n"
        ~ "  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk"
    );

    import core.stdc.stdlib : exit;
    exit(1);
}

void run(string[] args)
{
    if (args.length < 2)
        usage();

    string remotePath = args[0];
    string localPath = args[1];
    string spriteName = args.length > 2 ? args[2] : "";

    string sArgs = spriteName.length > 0
        ? format!"-s %s"(spriteName) : "";

    // Check if remote is directory or file
    string checkCmd = format!"sprite exec %s bash -c %s"(
        sArgs,
        shellQuote(format!"[ -d '%s' ] && echo dir || echo file"(remotePath)));
    auto checkResult = executeShell(checkCmd);
    string checkOutput = checkResult.output.strip();

    bool isRemoteDir = (checkOutput == "dir");

    if (isRemoteDir)
    {
        writefln("Pulling directory: %s -> %s", remotePath, localPath);
        mkdirRecurse(localPath);

        // sprite exec [-s name] tar czf - -C remotePath . | tar xzf - -C localPath
        string shellCmd = format!"sprite exec %s tar czf - -C %s . | tar xzf - -C %s"(
            sArgs,
            shellQuote(remotePath),
            shellQuote(localPath));
        auto result = executeShell(shellCmd);
        if (result.status != 0)
        {
            stderr.writeln("Error: pull failed");
            import core.stdc.stdlib : exit;
            exit(1);
        }
    }
    else
    {
        writefln("Pulling file: %s -> %s", remotePath, localPath);

        string localDir = dirName(localPath);
        if (localDir.length > 0 && localDir != ".")
        {
            mkdirRecurse(localDir);
        }

        // sprite exec [-s name] cat remotePath > localPath
        string shellCmd = format!"sprite exec %s cat %s > %s"(
            sArgs,
            shellQuote(remotePath),
            shellQuote(localPath));
        auto result = executeShell(shellCmd);
        if (result.status != 0)
        {
            stderr.writeln("Error: pull failed");
            import core.stdc.stdlib : exit;
            exit(1);
        }
    }

    writeln("Done.");
}
