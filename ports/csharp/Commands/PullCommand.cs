using System;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;

namespace SpriteTool.Commands;

public static class PullCommand
{
    public static int Execute(string remotePath, string localPath, string? spriteName)
    {
        var spriteArgs = !string.IsNullOrEmpty(spriteName)
            ? new[] { "-s", spriteName }
            : Array.Empty<string>();

        // Check if remote path is a directory
        var isDir = CheckIsDirectory(remotePath, spriteArgs);

        if (isDir)
        {
            Console.WriteLine($"Pulling directory: {remotePath} -> {localPath}");
            PullDirectory(remotePath, localPath, spriteArgs);
        }
        else
        {
            Console.WriteLine($"Pulling file: {remotePath} -> {localPath}");
            PullFile(remotePath, localPath, spriteArgs);
        }

        Console.WriteLine("Done.");
        return 0;
    }

    private static bool CheckIsDirectory(string remotePath, string[] spriteArgs)
    {
        var psi = new ProcessStartInfo
        {
            FileName = "sprite",
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
        };
        psi.ArgumentList.Add("exec");
        foreach (var arg in spriteArgs)
            psi.ArgumentList.Add(arg);
        psi.ArgumentList.Add("bash");
        psi.ArgumentList.Add("-c");
        psi.ArgumentList.Add($"[ -d '{remotePath}' ] && echo 'dir' || echo 'file'");

        using var proc = Process.Start(psi);
        var output = proc!.StandardOutput.ReadToEnd().Trim();
        proc.WaitForExit();
        return output == "dir";
    }

    private static void PullDirectory(string remotePath, string localPath, string[] spriteArgs)
    {
        Directory.CreateDirectory(localPath);

        // sprite exec [args] tar czf - -C remotePath .
        var spritePsi = new ProcessStartInfo
        {
            FileName = "sprite",
            UseShellExecute = false,
            RedirectStandardOutput = true,
        };
        spritePsi.ArgumentList.Add("exec");
        foreach (var arg in spriteArgs)
            spritePsi.ArgumentList.Add(arg);
        spritePsi.ArgumentList.Add("tar");
        spritePsi.ArgumentList.Add("czf");
        spritePsi.ArgumentList.Add("-");
        spritePsi.ArgumentList.Add("-C");
        spritePsi.ArgumentList.Add(remotePath);
        spritePsi.ArgumentList.Add(".");

        // local tar xzf - -C localPath
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

    private static void PullFile(string remotePath, string localPath, string[] spriteArgs)
    {
        var localDir = Path.GetDirectoryName(Path.GetFullPath(localPath));
        if (!string.IsNullOrEmpty(localDir))
            Directory.CreateDirectory(localDir);

        // sprite exec [args] cat remotePath
        var psi = new ProcessStartInfo
        {
            FileName = "sprite",
            UseShellExecute = false,
            RedirectStandardOutput = true,
        };
        psi.ArgumentList.Add("exec");
        foreach (var arg in spriteArgs)
            psi.ArgumentList.Add(arg);
        psi.ArgumentList.Add("cat");
        psi.ArgumentList.Add(remotePath);

        using var proc = Process.Start(psi);
        using (var fs = File.Create(localPath))
        {
            proc!.StandardOutput.BaseStream.CopyTo(fs);
        }
        proc.WaitForExit();
    }
}
