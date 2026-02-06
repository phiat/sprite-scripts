module SpriteTool.Pull

open System
open System.Diagnostics
open System.IO

let private usage () =
    printfn
        "Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]\n\
         \n\
         Examples:\n\
         \  sprite-tool pull /home/sprite/file.txt ./file.txt\n\
         \  sprite-tool pull /home/sprite/mydir ./mydir\n\
         \  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk"

    exit 1

let run (args: string list) =
    if args.Length < 2 then
        usage ()

    let remotePath = args.[0]
    let localPath = args.[1]

    let spriteName =
        if args.Length > 2 then Some args.[2] else None

    let sArgs = Sprite.spriteArgs spriteName

    // Check if remote is directory or file
    let checkPsi = ProcessStartInfo("sprite")
    checkPsi.UseShellExecute <- false
    checkPsi.RedirectStandardOutput <- true
    checkPsi.RedirectStandardError <- true

    for a in
        Array.append
            [| "exec" |]
            (Array.append sArgs
                [| "bash"
                   "-c"
                   sprintf "[ -d '%s' ] && echo dir || echo file" remotePath |]) do
        checkPsi.ArgumentList.Add a

    use checkProc = Process.Start checkPsi
    let checkOutput = checkProc.StandardOutput.ReadToEnd().Trim()
    checkProc.WaitForExit()

    let isDir = (checkOutput = "dir")

    if isDir then
        printfn "Pulling directory: %s -> %s" remotePath localPath
        Directory.CreateDirectory localPath |> ignore

        // sprite exec [-s name] tar czf - -C remotePath .
        let spritePsi = ProcessStartInfo("sprite")
        spritePsi.UseShellExecute <- false
        spritePsi.RedirectStandardOutput <- true

        for a in
            Array.append
                [| "exec" |]
                (Array.append sArgs [| "tar"; "czf"; "-"; "-C"; remotePath; "." |]) do
            spritePsi.ArgumentList.Add a

        use spriteProc = Process.Start spritePsi

        // tar xzf - -C localPath
        let tarPsi = ProcessStartInfo("tar")
        tarPsi.UseShellExecute <- false
        tarPsi.RedirectStandardInput <- true

        for a in [| "xzf"; "-"; "-C"; localPath |] do
            tarPsi.ArgumentList.Add a

        use tarProc = Process.Start tarPsi

        spriteProc.StandardOutput.BaseStream.CopyTo(tarProc.StandardInput.BaseStream)
        tarProc.StandardInput.Close()
        tarProc.WaitForExit()
        spriteProc.WaitForExit()

        if tarProc.ExitCode <> 0 then
            eprintfn "Error: pull failed"
            exit 1
    else
        printfn "Pulling file: %s -> %s" remotePath localPath

        let localDir = Path.GetDirectoryName(Path.GetFullPath localPath)

        if not (String.IsNullOrEmpty localDir) then
            Directory.CreateDirectory localDir |> ignore

        // sprite exec [-s name] cat remotePath > localPath
        let spritePsi = ProcessStartInfo("sprite")
        spritePsi.UseShellExecute <- false
        spritePsi.RedirectStandardOutput <- true

        for a in
            Array.append
                [| "exec" |]
                (Array.append sArgs [| "cat"; remotePath |]) do
            spritePsi.ArgumentList.Add a

        use spriteProc = Process.Start spritePsi

        use fileStream = File.Create localPath
        spriteProc.StandardOutput.BaseStream.CopyTo(fileStream)
        spriteProc.WaitForExit()

        if spriteProc.ExitCode <> 0 then
            eprintfn "Error: pull failed"
            exit 1

    printfn "Done."
