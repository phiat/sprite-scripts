module SpriteTool.Program

open System

[<EntryPoint>]
let main (argv: string array) =
    let args = argv |> Array.toList

    match args with
    | [] ->
        printfn "Usage: sprite-tool <command> [args...]"
        printfn ""
        printfn "Commands:"
        printfn "  launch    Create and configure a sprite with coding agent"
        printfn "  push      Push local file or directory to a sprite"
        printfn "  pull      Pull file or directory from a sprite"
        printfn "  watch     Poll a sprite's beads tracker task for progress"
        1
    | command :: rest ->
        match command.ToLowerInvariant() with
        | "launch" ->
            Launch.run rest
            0
        | "push" ->
            Push.run rest
            0
        | "pull" ->
            Pull.run rest
            0
        | "watch" ->
            Watch.run rest
            0
        | other ->
            eprintfn "Unknown command: %s" other
            eprintfn "Available commands: launch, push, pull, watch"
            1
