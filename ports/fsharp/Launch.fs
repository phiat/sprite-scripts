module SpriteTool.Launch

open System
open System.Diagnostics
open System.IO
open System.Threading

let private usage () =
    printfn
        "Usage: sprite-tool launch [options] <sprite-name> [plan-file]\n\
         \n\
         Options:\n\
         \  --dry-run              Show what would happen without executing\n\
         \  --no-checkpoint        Disable auto-checkpointing\n\
         \  --upload <dir>         Upload a local directory to /home/sprite/<dirname>\n\
         \                         (repeatable: --upload ./data --upload ./tests)\n\
         \n\
         Environment variables:\n\
         \  ENV_FILE               Path to .env file (default: ./.env)\n\
         \  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)\n\
         \  AGENT                  \"opencode\" (default) or \"claude\"\n\
         \  CLAUDE_AUTH            \"subscription\" (default) or \"apikey\"\n\
         \  MODEL                  Model override (see below)\n\
         \  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)\n\
         \n\
         Model examples:\n\
         \  OpenCode: MODEL=opencode/big-pickle  (free, default)\n\
         \            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)\n\
         \            MODEL=openai/gpt-4o\n\
         \            MODEL=google/gemini-2.5-pro\n\
         \  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku\n\
         \n\
         Examples:\n\
         \  sprite-tool launch my-project plan.md\n\
         \  sprite-tool launch --upload ./data my-project plan.md\n\
         \  sprite-tool launch --upload ./data --upload ./tests my-project plan.md\n\
         \  sprite-tool launch --dry-run my-project plan.md\n\
         \  MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md\n\
         \  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md"

    exit 1

/// Background checkpoint loop using async computation and CancellationToken.
type CheckpointLoop(spriteName: string, interval: int) =
    let cts = new CancellationTokenSource()

    member _.Start() =
        let token = cts.Token

        let loop =
            async {
                while not token.IsCancellationRequested do
                    do! Async.Sleep(interval * 1000)

                    if not token.IsCancellationRequested then
                        let now = DateTime.Now.ToString("HH:mm:ss")
                        printfn "[checkpoint] Creating checkpoint at %s..." now

                        try
                            let psi = ProcessStartInfo("sprite")
                            psi.UseShellExecute <- false
                            psi.RedirectStandardOutput <- true
                            psi.RedirectStandardError <- true

                            for a in [| "checkpoint"; "create"; "-s"; spriteName |] do
                                psi.ArgumentList.Add a

                            use proc = Process.Start psi
                            proc.WaitForExit()

                            if proc.ExitCode = 0 then
                                printfn "[checkpoint] Done."
                            else
                                printfn "[checkpoint] Failed (non-fatal)."
                        with _ ->
                            printfn "[checkpoint] Failed (non-fatal)."
            }

        Async.Start(loop, token)
        printfn "Auto-checkpointing every %ds (async)" interval

    member _.Stop() =
        if not cts.IsCancellationRequested then
            cts.Cancel()

    interface IDisposable with
        member this.Dispose() = this.Stop()

let run (args: string list) =
    // Load config from .env + environment
    let cfg = Config.load ()

    // Parse flags
    let mutable dryRun = false
    let mutable checkpointing = true
    let mutable uploadDirs: string list = []
    let mutable positional: string list = []
    let mutable i = 0
    let argsArr = args |> List.toArray

    while i < argsArr.Length do
        match argsArr.[i] with
        | "--dry-run" ->
            dryRun <- true
            i <- i + 1
        | "--no-checkpoint" ->
            checkpointing <- false
            i <- i + 1
        | "--upload" ->
            i <- i + 1

            if i >= argsArr.Length then
                eprintfn "Error: --upload requires an argument"
                exit 1

            uploadDirs <- uploadDirs @ [ argsArr.[i] ]
            i <- i + 1
        | "--help"
        | "-h" -> usage ()
        | s when s.StartsWith "--" ->
            eprintfn "Unknown option: %s" s
            usage ()
        | other ->
            positional <- positional @ [ other ]
            i <- i + 1

    if positional.Length < 1 then
        usage ()

    let spriteName = positional.[0]

    let planFile =
        if positional.Length > 1 then
            positional.[1]
        else
            ""

    // Shortcut helpers
    let sx cmd = Sprite.sx spriteName cmd dryRun |> ignore
    let sxPass cmd = Sprite.sxPassthrough spriteName cmd dryRun |> ignore

    let pushFile src dest =
        Sprite.pushFile spriteName src dest dryRun

    let pushDir src dest =
        Sprite.pushDir spriteName src dest dryRun

    // 1. Check/install sprite CLI
    let spriteOnPath =
        try
            let psi = ProcessStartInfo("which")
            psi.UseShellExecute <- false
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.ArgumentList.Add "sprite"
            use proc = Process.Start psi
            proc.WaitForExit()
            proc.ExitCode = 0
        with _ ->
            false

    if not spriteOnPath then
        if dryRun then
            printfn "  [dry-run] Would install sprite CLI"
        else
            printfn "Installing sprite CLI..."

            let psi = ProcessStartInfo("bash")
            psi.UseShellExecute <- false
            psi.ArgumentList.Add "-c"
            psi.ArgumentList.Add "curl -fsSL https://sprites.dev/install.sh | sh"
            use proc = Process.Start psi
            proc.WaitForExit()

            let localBin =
                Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".local", "bin")

            let currentPath = Environment.GetEnvironmentVariable "PATH"
            Environment.SetEnvironmentVariable("PATH", sprintf "%s:%s" localBin currentPath)

    // 2. Auth sprite
    if not (String.IsNullOrEmpty cfg.SpriteToken) then
        printfn "Authenticating sprite with token..."

        if not dryRun then
            let psi = ProcessStartInfo("sprite")
            psi.UseShellExecute <- false

            for a in [| "auth"; "setup"; "--token"; cfg.SpriteToken |] do
                psi.ArgumentList.Add a

            use proc = Process.Start psi
            proc.WaitForExit()
    else
        printfn "No SPRITE_TOKEN set. Running interactive login..."

        if not dryRun then
            let psi = ProcessStartInfo("sprite")
            psi.UseShellExecute <- false
            psi.ArgumentList.Add "login"
            use proc = Process.Start psi
            proc.WaitForExit()

    // 3. Create sprite (or use existing)
    if dryRun then
        printfn "  [dry-run] Would create or reuse sprite '%s'" spriteName
    elif Sprite.spriteExists spriteName then
        printfn "Sprite '%s' already exists, using it." spriteName
    else
        printfn "Creating sprite: %s" spriteName

        let psi = ProcessStartInfo("sprite")
        psi.UseShellExecute <- false

        for a in [| "create"; "-skip-console"; spriteName |] do
            psi.ArgumentList.Add a

        use proc = Process.Start psi
        proc.WaitForExit()

    // 4. Push .env to sprite
    if File.Exists cfg.EnvFile then
        printfn "Pushing %s..." cfg.EnvFile
        pushFile cfg.EnvFile "/home/sprite/.env"

    // 5. Push plan file if provided
    if not (String.IsNullOrEmpty planFile) && File.Exists planFile then
        printfn "Pushing %s..." planFile
        pushFile planFile "/home/sprite/plan.md"

    // 6. Upload directories if provided
    uploadDirs
    |> List.iter (fun dir ->
        if Directory.Exists dir then
            let dirName = Path.GetFileName(Path.GetFullPath dir)
            printfn "Uploading directory: %s -> /home/sprite/%s" dir dirName
            pushDir dir (sprintf "/home/sprite/%s" dirName)
        else
            printfn "WARNING: --upload dir '%s' not found, skipping." dir)

    // 7. Setup git + beads
    printfn "Initializing git..."
    sx "cd /home/sprite && git init -b main 2>/dev/null || true"

    printfn "Installing beads..."
    sx "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash"

    // 8. Install and auth coding agent
    if cfg.Agent = "claude" then
        printfn "Setting up claude..."
        sx "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code"

        if cfg.ClaudeAuth = "subscription" then
            let credsPath =
                Path.Combine(
                    Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
                    ".claude",
                    ".credentials.json"
                )

            if File.Exists credsPath then
                printfn "Copying claude subscription credentials..."
                pushFile credsPath "/home/sprite/.claude/.credentials.json"
                sx "chmod 600 ~/.claude/.credentials.json"
            else
                eprintfn "ERROR: ~/.claude/.credentials.json not found"
                eprintfn "Run 'claude' locally first to authenticate, then re-run this script."
                exit 1
        elif cfg.ClaudeAuth = "apikey" && not (String.IsNullOrEmpty cfg.AnthropicApiKey) then
            printfn "Setting ANTHROPIC_API_KEY in sprite..."

            sx
                (sprintf
                    "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"%s\"' >> ~/.bashrc"
                    cfg.AnthropicApiKey)
        else
            eprintfn "ERROR: No valid claude auth configured"
            eprintfn "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY"
            exit 1
    elif cfg.Agent = "opencode" then
        printfn "Setting up opencode..."
        sx "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash"

        sx
            "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc"
    else
        eprintfn "ERROR: Unknown AGENT '%s'. Use 'claude' or 'opencode'." cfg.Agent
        exit 1

    // 9. Launch agent with plan (or open console)
    printfn ""
    printfn "=========================================="
    printfn "Sprite '%s' is ready!" spriteName

    let modelNote =
        if String.IsNullOrEmpty cfg.Model then
            ""
        else
            sprintf " (model: %s)" cfg.Model

    printfn "Agent: %s%s" cfg.Agent modelNote

    if checkpointing then
        printfn "Checkpointing: every %ds" cfg.CheckpointInterval

    printfn "=========================================="

    if dryRun then
        printfn ""
        printfn "[dry-run] Would launch %s with plan. No changes were made." cfg.Agent
    elif not (String.IsNullOrEmpty planFile) then
        // Start auto-checkpointing before agent runs
        let mutable checkpointLoop: CheckpointLoop option = None

        if checkpointing then
            let loop = new CheckpointLoop(spriteName, cfg.CheckpointInterval)
            loop.Start()
            checkpointLoop <- Some loop

        printfn "Launching %s with plan..." cfg.Agent

        if cfg.Agent = "claude" then
            let modelFlag =
                if String.IsNullOrEmpty cfg.Model then
                    ""
                else
                    sprintf "--model %s " cfg.Model

            sxPass (sprintf "cd /home/sprite && claude %s-p 'read plan.md and complete the plan please'" modelFlag)
        elif cfg.Agent = "opencode" then
            let ocModel =
                if String.IsNullOrEmpty cfg.Model then
                    "opencode/big-pickle"
                else
                    cfg.Model

            sxPass
                (sprintf
                    "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m %s 'read plan.md and complete the plan please'"
                    ocModel)

        // Final checkpoint after agent completes
        checkpointLoop |> Option.iter (fun loop -> loop.Stop())

        printfn "Creating final checkpoint..."

        let psi = ProcessStartInfo("sprite")
        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true

        for a in [| "checkpoint"; "create"; "-s"; spriteName |] do
            psi.ArgumentList.Add a

        use proc = Process.Start psi
        proc.WaitForExit()

        if proc.ExitCode = 0 then
            printfn "Final checkpoint saved."
        else
            printfn "Final checkpoint failed (non-fatal)."
    else
        printfn "Opening console..."

        let psi = ProcessStartInfo("sprite")
        psi.UseShellExecute <- false

        for a in [| "console"; "-s"; spriteName |] do
            psi.ArgumentList.Add a

        use proc = Process.Start psi
        proc.WaitForExit()
