module sprite;

import std.algorithm : canFind, splitter;
import std.array : array;
import std.format : format;
import std.process : executeShell, spawnShell, wait;
import std.stdio : writeln, writefln, stderr;
import std.string : strip, splitLines;

/// Build sprite-args string fragment: "-s name" when a sprite name is given.
string spriteArgsStr(string spriteName)
{
    if (spriteName.length > 0)
        return format!"-s %s"(spriteName);
    return "";
}

/// Build sprite exec args array with optional -s flag.
string[] spriteExecArgs(string spriteName)
{
    if (spriteName.length > 0)
        return ["exec", "-s", spriteName];
    return ["exec"];
}

/// Run a command inside a sprite via bash. Returns captured stdout (trimmed).
/// Equivalent to: sprite exec -s SPRITE bash -c "CMD"
string sx(string spriteName, string cmd, bool dryRun)
{
    if (dryRun)
    {
        writefln("  [dry-run] sprite exec -s %s bash -c \"%s\"", spriteName, cmd);
        return "";
    }

    string fullCmd = format!"sprite exec -s %s bash -c %s"(
        spriteName, shellQuote(cmd));
    auto result = executeShell(fullCmd);
    return result.output.strip();
}

/// Run a command inside a sprite, passing stdout/stderr through to the terminal.
/// Returns the exit code.
int sxPassthrough(string spriteName, string cmd, bool dryRun)
{
    if (dryRun)
    {
        writefln("  [dry-run] sprite exec -s %s bash -c \"%s\"", spriteName, cmd);
        return 0;
    }

    string fullCmd = format!"sprite exec -s %s bash -c %s"(
        spriteName, shellQuote(cmd));
    auto pid = spawnShell(fullCmd);
    return pid.wait();
}

/// Push a local file to a sprite via stdin pipe.
void pushFile(string spriteName, string src, string dest, bool dryRun)
{
    if (dryRun)
    {
        writefln("  [dry-run] push %s -> sprite:%s", src, dest);
        return;
    }

    import std.path : dirName;

    string destDir = dirName(dest);
    sx(spriteName, format!"mkdir -p '%s'"(destDir), false);

    string shellCmd = format!"sprite exec -s %s bash -c %s < %s"(
        spriteName, shellQuote(format!"cat > '%s'"(dest)), shellQuote(src));
    auto result = executeShell(shellCmd);
    if (result.status != 0)
    {
        stderr.writefln("Error: push file failed");
    }
}

/// Push a local directory to a sprite via tar pipe.
void pushDir(string spriteName, string src, string dest, bool dryRun)
{
    if (dryRun)
    {
        writefln("  [dry-run] push dir %s -> sprite:%s", src, dest);
        return;
    }

    import std.path : absolutePath, dirName, baseName;

    string fullPath = absolutePath(src);
    string parent = dirName(fullPath);
    string base = baseName(fullPath);

    sx(spriteName, format!"mkdir -p '%s'"(dest), false);

    string shellCmd = format!"tar czf - -C %s %s | sprite exec -s %s bash -c %s"(
        shellQuote(parent), shellQuote(base), spriteName,
        shellQuote(format!"tar xzf - -C '%s'"(dirName(dest))));
    auto result = executeShell(shellCmd);
    if (result.status != 0)
    {
        stderr.writefln("Error: push dir failed");
    }
}

/// Run 'sprite ls' and check if a sprite with the given name already exists.
bool spriteExists(string spriteName)
{
    auto result = executeShell("sprite ls 2>/dev/null");
    if (result.status != 0)
        return false;

    foreach (line; result.output.splitLines())
    {
        foreach (word; line.splitter())
        {
            if (word.strip() == spriteName)
                return true;
        }
    }
    return false;
}

/// Shell-quote a string for safe embedding in shell commands.
string shellQuote(string s)
{
    import std.array : replace;
    return "'" ~ s.replace("'", "'\\''") ~ "'";
}
