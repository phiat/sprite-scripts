using System;
using System.IO;

namespace SpriteTool.Commands;

public static class PushCommand
{
    public static int Execute(string localPath, string remotePath, string? spriteName)
    {
        // Resolve sprite name: argument, or let sprite CLI use default
        var sprite = spriteName ?? "";

        if (!File.Exists(localPath) && !Directory.Exists(localPath))
        {
            Console.Error.WriteLine($"Error: {localPath} does not exist");
            return 1;
        }

        if (Directory.Exists(localPath))
        {
            Console.WriteLine($"Pushing directory: {localPath} -> {remotePath}");
            if (string.IsNullOrEmpty(sprite))
            {
                PushDirNoSprite(localPath, remotePath);
            }
            else
            {
                SpriteExec.PushDirStrip(sprite, localPath, remotePath);
            }
        }
        else
        {
            Console.WriteLine($"Pushing file: {localPath} -> {remotePath}");
            if (string.IsNullOrEmpty(sprite))
            {
                PushFileNoSprite(localPath, remotePath);
            }
            else
            {
                SpriteExec.PushFile(sprite, localPath, remotePath);
            }
        }

        Console.WriteLine("Done.");
        return 0;
    }

    /// <summary>
    /// Push file without -s flag (uses default sprite).
    /// </summary>
    private static void PushFileNoSprite(string localPath, string remotePath)
    {
        var remoteDir = Path.GetDirectoryName(remotePath)?.Replace('\\', '/') ?? "/";

        var mkdirPsi = new System.Diagnostics.ProcessStartInfo
        {
            FileName = "sprite",
            ArgumentList = { "exec", "bash", "-c", $"mkdir -p '{remoteDir}' && cat > '{remotePath}'" },
            UseShellExecute = false,
            RedirectStandardInput = true,
        };
        using var proc = System.Diagnostics.Process.Start(mkdirPsi);
        using (var input = proc!.StandardInput.BaseStream)
        {
            using var fs = File.OpenRead(localPath);
            fs.CopyTo(input);
        }
        proc.WaitForExit();
    }

    /// <summary>
    /// Push directory without -s flag (uses default sprite), with --strip-components=1.
    /// </summary>
    private static void PushDirNoSprite(string localDir, string remoteDir)
    {
        var parentDir = Path.GetDirectoryName(Path.GetFullPath(localDir)) ?? ".";
        var baseName = Path.GetFileName(Path.GetFullPath(localDir));

        var tarPsi = new System.Diagnostics.ProcessStartInfo
        {
            FileName = "tar",
            ArgumentList = { "czf", "-", "-C", parentDir, baseName },
            UseShellExecute = false,
            RedirectStandardOutput = true,
        };

        var spritePsi = new System.Diagnostics.ProcessStartInfo
        {
            FileName = "sprite",
            ArgumentList = { "exec", "bash", "-c",
                $"mkdir -p '{remoteDir}' && tar xzf - -C '{remoteDir}' --strip-components=1" },
            UseShellExecute = false,
            RedirectStandardInput = true,
        };

        using var tarProc = System.Diagnostics.Process.Start(tarPsi);
        using var spriteProc = System.Diagnostics.Process.Start(spritePsi);

        var copyTask = System.Threading.Tasks.Task.Run(() =>
        {
            using var tarOut = tarProc!.StandardOutput.BaseStream;
            using var spriteIn = spriteProc!.StandardInput.BaseStream;
            tarOut.CopyTo(spriteIn);
        });

        copyTask.Wait();
        tarProc!.WaitForExit();
        spriteProc!.WaitForExit();
    }
}
