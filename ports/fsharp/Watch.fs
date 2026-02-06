module SpriteTool.Watch

open System
open System.Threading

let private usage () =
    printfn
        "Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]\n\
         \n\
         Arguments:\n\
         \  sprite-name     Name of the sprite to watch\n\
         \  task-id         Beads task ID to track (default: auto-detect first open critical task)\n\
         \  poll-interval   Seconds between polls (default: 30)\n\
         \n\
         Examples:\n\
         \  sprite-tool watch ember-red-hawk\n\
         \  sprite-tool watch ember-red-hawk CRM-1\n\
         \  sprite-tool watch ember-red-hawk CRM-1 60"

    exit 1

/// Run command inside sprite, capture stdout (suppresses stderr).
let private sxCapture (spriteName: string) (cmd: string) =
    Sprite.sx spriteName cmd false
    |> Option.defaultValue ""

/// Run command inside sprite, print output, return it.
let private sxPrint (spriteName: string) (cmd: string) =
    let output = sxCapture spriteName cmd

    if not (String.IsNullOrEmpty output) then
        printfn "%s" output

    output

let run (args: string list) =
    if args.Length < 1 then
        usage ()

    let spriteName = args.[0]

    let mutable taskId =
        if args.Length > 1 then args.[1] else ""

    let pollInterval =
        if args.Length > 2 then
            match Int32.TryParse args.[2] with
            | true, v -> v
            | false, _ ->
                eprintfn "Error: invalid poll-interval '%s' (must be integer)" args.[2]
                exit 1
        else
            30

    // Auto-detect tracker task if not specified
    if String.IsNullOrEmpty taskId then
        printfn "Detecting tracker task..."

        taskId <-
            sxCapture
                spriteName
                "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'"

        if String.IsNullOrEmpty taskId then
            printfn "No critical task found. Falling back to first open task..."

            taskId <-
                sxCapture spriteName "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'"

        if String.IsNullOrEmpty taskId then
            eprintfn "ERROR: No beads tasks found on sprite '%s'" spriteName
            eprintfn "Specify a task ID manually: sprite-tool watch %s <task-id>" spriteName
            exit 1

        printfn "Tracking task: %s" taskId

    printfn "Watching sprite '%s' task '%s' (every %ds)" spriteName taskId pollInterval
    printfn "Press Ctrl+C to stop"
    printfn ""

    let cts = new CancellationTokenSource()

    Console.CancelKeyPress.Add(fun evtArgs ->
        evtArgs.Cancel <- true
        cts.Cancel())

    try
        while not cts.Token.IsCancellationRequested do
            // Clear screen using ANSI escape
            printf "\x1B[2J\x1B[H"
            Console.Out.Flush()

            let now = DateTime.Now.ToString("HH:mm:ss")
            printfn "=== sprite-watch: %s / %s === %s ===" spriteName taskId now
            printfn ""

            // Show task status
            let taskOutput =
                sxPrint spriteName (sprintf "cd /home/sprite && bd show %s 2>/dev/null" taskId)

            if String.IsNullOrEmpty taskOutput then
                printfn "(could not read task)"

            printfn ""

            // Show recent comments
            printfn "--- Recent updates ---"

            let commentsOutput =
                sxPrint spriteName (sprintf "cd /home/sprite && bd comments %s 2>/dev/null | tail -8" taskId)

            if String.IsNullOrEmpty commentsOutput then
                printfn "(no comments)"

            printfn ""

            // Check if done
            let status =
                sxCapture spriteName (sprintf "cd /home/sprite && bd show %s 2>/dev/null | grep -i status" taskId)

            if not (String.IsNullOrEmpty status) then
                let statusLower = status.ToLowerInvariant()

                if
                    statusLower.Contains "closed"
                    || statusLower.Contains "done"
                    || statusLower.Contains "completed"
                then
                    printfn "=========================================="
                    printfn "PROJECT COMPLETE"
                    printfn "=========================================="
                    cts.Cancel()
                else
                    Thread.Sleep(pollInterval * 1000)
            else
                Thread.Sleep(pollInterval * 1000)
    with :? OperationCanceledException ->
        ()

    printfn ""
    printfn "Stopped."
