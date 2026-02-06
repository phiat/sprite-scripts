module SpriteTool.Push

open System
open System.Diagnostics
open System.IO

let private usage () =
    printfn
        "Usage: sprite-tool push <local-path> <remote-path> [sprite-name]\n\
         \n\
         Examples:\n\
         \  sprite-tool push ./file.txt /home/sprite/file.txt\n\
         \  sprite-tool push ./mydir /home/sprite/mydir\n\
         \  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk"

    exit 1

let run (args: string list) =
    if args.Length < 2 then
        usage ()

    let localPath = args.[0]
    let remotePath = args.[1]

    let spriteName =
        if args.Length > 2 then Some args.[2] else None

    let sArgs = Sprite.spriteArgs spriteName

    if not (File.Exists localPath) && not (Directory.Exists localPath) then
        eprintfn "Error: %s does not exist" localPath
        exit 1

    if Directory.Exists localPath then
        printfn "Pushing directory: %s -> %s" localPath remotePath

        let fullPath = Path.GetFullPath localPath
        let parent = Path.GetDirectoryName fullPath
        let baseName = Path.GetFileName fullPath

        // tar czf - -C parent baseName
        let tarPsi = ProcessStartInfo("tar")
        tarPsi.UseShellExecute <- false
        tarPsi.RedirectStandardOutput <- true

        for a in [| "czf"; "-"; "-C"; parent; baseName |] do
            tarPsi.ArgumentList.Add a

        use tarProc = Process.Start tarPsi

        // sprite exec [-s name] bash -c "mkdir -p DEST && tar xzf - -C DEST --strip-components=1"
        let spritePsi = ProcessStartInfo("sprite")
        spritePsi.UseShellExecute <- false
        spritePsi.RedirectStandardInput <- true

        for a in
            Array.append
                [| "exec" |]
                (Array.append sArgs
                    [| "bash"
                       "-c"
                       sprintf "mkdir -p '%s' && tar xzf - -C '%s' --strip-components=1" remotePath remotePath |]) do
            spritePsi.ArgumentList.Add a

        use spriteProc = Process.Start spritePsi

        tarProc.StandardOutput.BaseStream.CopyTo(spriteProc.StandardInput.BaseStream)
        spriteProc.StandardInput.Close()
        spriteProc.WaitForExit()
        tarProc.WaitForExit()

        if spriteProc.ExitCode <> 0 then
            eprintfn "Error: push failed"
            exit 1
    else
        printfn "Pushing file: %s -> %s" localPath remotePath

        let remoteDir = Path.GetDirectoryName remotePath

        let spritePsi = ProcessStartInfo("sprite")
        spritePsi.UseShellExecute <- false
        spritePsi.RedirectStandardInput <- true

        for a in
            Array.append
                [| "exec" |]
                (Array.append sArgs
                    [| "bash"
                       "-c"
                       sprintf "mkdir -p '%s' && cat > '%s'" remoteDir remotePath |]) do
            spritePsi.ArgumentList.Add a

        use spriteProc = Process.Start spritePsi
        use fileStream = File.OpenRead localPath
        fileStream.CopyTo(spriteProc.StandardInput.BaseStream)
        spriteProc.StandardInput.Close()
        spriteProc.WaitForExit()

        if spriteProc.ExitCode <> 0 then
            eprintfn "Error: push failed"
            exit 1

    printfn "Done."
