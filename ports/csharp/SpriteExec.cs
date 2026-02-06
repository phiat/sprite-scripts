using System;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;

namespace SpriteTool;

/// <summary>
/// Process wrapper for executing sprite CLI commands.
/// </summary>
public static class SpriteExec
{
    /// <summary>
    /// Run a bash command inside a sprite via "sprite exec -s SPRITE bash -c CMD".
    /// Returns the process exit code.
    /// </summary>
    public static int Sx(string sprite, string command, bool dryRun = false)
    {
        if (dryRun)
        {
            Console.WriteLine($"  [dry-run] sprite exec -s {sprite} bash -c \"{command}\"");
            return 0;
        }

        var psi = new ProcessStartInfo
        {
            FileName = "sprite",
            ArgumentList = { "exec", "-s", sprite, "bash", "-c", command },
            UseShellExecute = false,
        };
        using var proc = Process.Start(psi);
        proc!.WaitForExit();
        return proc.ExitCode;
    }

    /// <summary>
    /// Run a bash command inside a sprite and capture stdout.
    /// </summary>
    public static (int exitCode, string output) SxCapture(string sprite, string command)
    {
        var psi = new ProcessStartInfo
        {
            FileName = "sprite",
            ArgumentList = { "exec", "-s", sprite, "bash", "-c", command },
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
        };
        using var proc = Process.Start(psi);
        var stdout = proc!.StandardOutput.ReadToEnd();
        proc.WaitForExit();
        return (proc.ExitCode, stdout.Trim());
    }

    /// <summary>
    /// Run a sprite command (not bash -c) and capture stdout.
    /// </summary>
    public static (int exitCode, string output) SpriteCapture(params string[] args)
    {
        var psi = new ProcessStartInfo
        {
            FileName = "sprite",
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
        };
        foreach (var arg in args)
            psi.ArgumentList.Add(arg);
        using var proc = Process.Start(psi);
        var stdout = proc!.StandardOutput.ReadToEnd();
        proc.WaitForExit();
        return (proc.ExitCode, stdout);
    }

    /// <summary>
    /// Run a sprite command (not bash -c) and wait for exit.
    /// </summary>
    public static int SpriteRun(params string[] args)
    {
        var psi = new ProcessStartInfo
        {
            FileName = "sprite",
            UseShellExecute = false,
        };
        foreach (var arg in args)
            psi.ArgumentList.Add(arg);
        using var proc = Process.Start(psi);
        proc!.WaitForExit();
        return proc.ExitCode;
    }

    /// <summary>
    /// Push a local file to a sprite by piping stdin.
    /// </summary>
    public static void PushFile(string sprite, string localPath, string remotePath, bool dryRun = false)
    {
        if (dryRun)
        {
            Console.WriteLine($"  [dry-run] push {localPath} -> sprite:{remotePath}");
            return;
        }

        var remoteDir = Path.GetDirectoryName(remotePath)?.Replace('\\', '/') ?? "/";
        Sx(sprite, $"mkdir -p '{remoteDir}'");

        var psi = new ProcessStartInfo
        {
            FileName = "sprite",
            ArgumentList = { "exec", "-s", sprite, "bash", "-c", $"cat > '{remotePath}'" },
            UseShellExecute = false,
            RedirectStandardInput = true,
        };
        using var proc = Process.Start(psi);
        using (var input = proc!.StandardInput.BaseStream)
        {
            using var fs = File.OpenRead(localPath);
            fs.CopyTo(input);
        }
        proc.WaitForExit();
    }

    /// <summary>
    /// Push a local directory to a sprite via tar pipe.
    /// </summary>
    public static void PushDir(string sprite, string localDir, string remoteDir, bool dryRun = false)
    {
        if (dryRun)
        {
            Console.WriteLine($"  [dry-run] push dir {localDir} -> sprite:{remoteDir}");
            return;
        }

        Sx(sprite, $"mkdir -p '{remoteDir}'");

        var parentDir = Path.GetDirectoryName(Path.GetFullPath(localDir)) ?? ".";
        var baseName = Path.GetFileName(Path.GetFullPath(localDir));

        // tar on local side
        var tarPsi = new ProcessStartInfo
        {
            FileName = "tar",
            ArgumentList = { "czf", "-", "-C", parentDir, baseName },
            UseShellExecute = false,
            RedirectStandardOutput = true,
        };

        // sprite exec on remote side
        var remoteDirParent = Path.GetDirectoryName(remoteDir)?.Replace('\\', '/') ?? "/";
        var spritePsi = new ProcessStartInfo
        {
            FileName = "sprite",
            ArgumentList = { "exec", "-s", sprite, "bash", "-c", $"tar xzf - -C '{remoteDirParent}'" },
            UseShellExecute = false,
            RedirectStandardInput = true,
        };

        using var tarProc = Process.Start(tarPsi);
        using var spriteProc = Process.Start(spritePsi);

        // Pipe tar stdout -> sprite stdin
        var copyTask = Task.Run(() =>
        {
            using var tarOut = tarProc!.StandardOutput.BaseStream;
            using var spriteIn = spriteProc!.StandardInput.BaseStream;
            tarOut.CopyTo(spriteIn);
        });

        copyTask.Wait();
        tarProc!.WaitForExit();
        spriteProc!.WaitForExit();
    }

    /// <summary>
    /// Push a local directory to a sprite via tar pipe (for push command with --strip-components).
    /// </summary>
    public static void PushDirStrip(string sprite, string localDir, string remoteDir, bool dryRun = false)
    {
        if (dryRun)
        {
            Console.WriteLine($"  [dry-run] push dir {localDir} -> sprite:{remoteDir}");
            return;
        }

        var parentDir = Path.GetDirectoryName(Path.GetFullPath(localDir)) ?? ".";
        var baseName = Path.GetFileName(Path.GetFullPath(localDir));

        // tar on local side
        var tarPsi = new ProcessStartInfo
        {
            FileName = "tar",
            ArgumentList = { "czf", "-", "-C", parentDir, baseName },
            UseShellExecute = false,
            RedirectStandardOutput = true,
        };

        // sprite exec on remote side
        var spritePsi = new ProcessStartInfo
        {
            FileName = "sprite",
            ArgumentList = { "exec", "-s", sprite, "bash", "-c",
                $"mkdir -p '{remoteDir}' && tar xzf - -C '{remoteDir}' --strip-components=1" },
            UseShellExecute = false,
            RedirectStandardInput = true,
        };

        using var tarProc = Process.Start(tarPsi);
        using var spriteProc = Process.Start(spritePsi);

        var copyTask = Task.Run(() =>
        {
            using var tarOut = tarProc!.StandardOutput.BaseStream;
            using var spriteIn = spriteProc!.StandardInput.BaseStream;
            tarOut.CopyTo(spriteIn);
        });

        copyTask.Wait();
        tarProc!.WaitForExit();
        spriteProc!.WaitForExit();
    }

    /// <summary>
    /// Pull a file from a sprite by capturing stdout.
    /// </summary>
    public static void PullFile(string sprite, string remotePath, string localPath)
    {
        var localDir = Path.GetDirectoryName(Path.GetFullPath(localPath));
        if (!string.IsNullOrEmpty(localDir))
            Directory.CreateDirectory(localDir);

        var psi = new ProcessStartInfo
        {
            FileName = "sprite",
            ArgumentList = { "exec", "-s", sprite, "cat", remotePath },
            UseShellExecute = false,
            RedirectStandardOutput = true,
        };
        using var proc = Process.Start(psi);
        using (var fs = File.Create(localPath))
        {
            proc!.StandardOutput.BaseStream.CopyTo(fs);
        }
        proc.WaitForExit();
    }

    /// <summary>
    /// Pull a directory from a sprite via tar pipe.
    /// </summary>
    public static void PullDir(string sprite, string remotePath, string localPath)
    {
        Directory.CreateDirectory(localPath);

        // sprite exec tar on remote side
        var spritePsi = new ProcessStartInfo
        {
            FileName = "sprite",
            ArgumentList = { "exec", "-s", sprite, "tar", "czf", "-", "-C", remotePath, "." },
            UseShellExecute = false,
            RedirectStandardOutput = true,
        };

        // tar on local side
        var tarPsi = new ProcessStartInfo
        {
            FileName = "tar",
            ArgumentList = { "xzf", "-", "-C", localPath },
            UseShellExecute = false,
            RedirectStandardInput = true,
        };

        using var spriteProc = Process.Start(spritePsi);
        using var tarProc = Process.Start(tarPsi);

        var copyTask = Task.Run(() =>
        {
            using var spriteOut = spriteProc!.StandardOutput.BaseStream;
            using var tarIn = tarProc!.StandardInput.BaseStream;
            spriteOut.CopyTo(tarIn);
        });

        copyTask.Wait();
        spriteProc!.WaitForExit();
        tarProc!.WaitForExit();
    }

    /// <summary>
    /// Check if the sprite CLI is installed.
    /// </summary>
    public static bool IsSpriteInstalled()
    {
        try
        {
            var psi = new ProcessStartInfo
            {
                FileName = "sprite",
                Arguments = "--version",
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
            };
            using var proc = Process.Start(psi);
            proc!.WaitForExit();
            return proc.ExitCode == 0;
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Install the sprite CLI.
    /// </summary>
    public static void InstallSprite()
    {
        Console.WriteLine("Installing sprite CLI...");
        var psi = new ProcessStartInfo
        {
            FileName = "bash",
            ArgumentList = { "-c", "curl -fsSL https://sprites.dev/install.sh | sh" },
            UseShellExecute = false,
        };
        using var proc = Process.Start(psi);
        proc!.WaitForExit();

        // Add to PATH
        var localBin = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".local", "bin");
        var currentPath = Environment.GetEnvironmentVariable("PATH") ?? "";
        if (!currentPath.Contains(localBin))
        {
            Environment.SetEnvironmentVariable("PATH", $"{localBin}:{currentPath}");
        }
    }
}
