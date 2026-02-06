module SpriteTool.Sprite

open System
open System.Diagnostics
open System.IO

/// Build sprite-args list: ["-s"; name] when a sprite name is given.
let spriteArgs (spriteName: string option) =
    match spriteName with
    | Some name -> [| "-s"; name |]
    | None -> [||]

/// Run a process and wait for it. Returns exit code.
let private runProcess (fileName: string) (args: string array) (redirectIn: bool) (redirectOut: bool) =
    let psi = ProcessStartInfo(fileName)
    psi.UseShellExecute <- false
    psi.RedirectStandardInput <- redirectIn
    psi.RedirectStandardOutput <- redirectOut
    psi.RedirectStandardError <- false

    for a in args do
        psi.ArgumentList.Add a

    use proc = Process.Start psi
    proc, proc.WaitForExit()

/// Run a command inside a sprite via bash. Returns captured stdout (trimmed).
/// Equivalent to: sprite exec -s SPRITE bash -c "CMD"
let sx (spriteName: string) (cmd: string) (dryRun: bool) : string option =
    if dryRun then
        printfn "  [dry-run] sprite exec -s %s bash -c \"%s\"" spriteName cmd
        None
    else
        let psi = ProcessStartInfo("sprite")
        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true

        for a in [| "exec"; "-s"; spriteName; "bash"; "-c"; cmd |] do
            psi.ArgumentList.Add a

        use proc = Process.Start psi
        let stdout = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        Some(stdout.Trim())

/// Run a command inside a sprite, passing stdout/stderr through to the terminal.
/// Returns the exit code.
let sxPassthrough (spriteName: string) (cmd: string) (dryRun: bool) : int =
    if dryRun then
        printfn "  [dry-run] sprite exec -s %s bash -c \"%s\"" spriteName cmd
        0
    else
        let psi = ProcessStartInfo("sprite")
        psi.UseShellExecute <- false

        for a in [| "exec"; "-s"; spriteName; "bash"; "-c"; cmd |] do
            psi.ArgumentList.Add a

        use proc = Process.Start psi
        proc.WaitForExit()
        proc.ExitCode

/// Push a local file to a sprite via stdin redirection.
let pushFile (spriteName: string) (src: string) (dest: string) (dryRun: bool) =
    if dryRun then
        printfn "  [dry-run] push %s -> sprite:%s" src dest
    else
        let destDir = Path.GetDirectoryName dest

        sx spriteName (sprintf "mkdir -p '%s'" destDir) false |> ignore

        let psi = ProcessStartInfo("sprite")
        psi.UseShellExecute <- false
        psi.RedirectStandardInput <- true

        for a in [| "exec"; "-s"; spriteName; "bash"; "-c"; sprintf "cat > '%s'" dest |] do
            psi.ArgumentList.Add a

        use proc = Process.Start psi
        use fileStream = File.OpenRead src
        fileStream.CopyTo(proc.StandardInput.BaseStream)
        proc.StandardInput.Close()
        proc.WaitForExit()

/// Push a local directory to a sprite via tar pipe.
let pushDir (spriteName: string) (src: string) (dest: string) (dryRun: bool) =
    if dryRun then
        printfn "  [dry-run] push dir %s -> sprite:%s" src dest
    else
        let parent = Path.GetDirectoryName(Path.GetFullPath src)
        let baseName = Path.GetFileName(Path.GetFullPath src)
        let destParent = Path.GetDirectoryName dest

        sx spriteName (sprintf "mkdir -p '%s'" dest) false |> ignore

        // tar czf - -C parent baseName
        let tarPsi = ProcessStartInfo("tar")
        tarPsi.UseShellExecute <- false
        tarPsi.RedirectStandardOutput <- true

        for a in [| "czf"; "-"; "-C"; parent; baseName |] do
            tarPsi.ArgumentList.Add a

        use tarProc = Process.Start tarPsi

        // sprite exec -s SPRITE bash -c "tar xzf - -C destParent"
        let spritePsi = ProcessStartInfo("sprite")
        spritePsi.UseShellExecute <- false
        spritePsi.RedirectStandardInput <- true

        for a in
            [| "exec"
               "-s"
               spriteName
               "bash"
               "-c"
               sprintf "tar xzf - -C '%s'" destParent |] do
            spritePsi.ArgumentList.Add a

        use spriteProc = Process.Start spritePsi

        tarProc.StandardOutput.BaseStream.CopyTo(spriteProc.StandardInput.BaseStream)
        spriteProc.StandardInput.Close()
        spriteProc.WaitForExit()
        tarProc.WaitForExit()

        if spriteProc.ExitCode <> 0 then
            eprintfn "Error: push dir failed"
            exit 1

/// Run 'sprite ls' and return stdout.
let spriteList () =
    let psi = ProcessStartInfo("sprite")
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true

    psi.ArgumentList.Add "ls"

    use proc = Process.Start psi
    let stdout = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    stdout

/// Check if a sprite with the given name already exists.
let spriteExists (spriteName: string) =
    let output = spriteList ()

    output.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.exists (fun line ->
        line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.contains spriteName)
