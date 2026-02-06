package main

import "core:fmt"
import "core:os"

main :: proc() {
    args := os.args

    if len(args) < 2 {
        print_usage()
        os.exit(1)
    }

    command := args[1]
    sub_args := args[2:]

    switch command {
    case "launch":
        cmd_launch(sub_args)
    case "push":
        cmd_push(sub_args)
    case "pull":
        cmd_pull(sub_args)
    case "watch":
        cmd_watch(sub_args)
    case "--help", "-h":
        print_usage()
    case:
        fmt.eprintf("Unknown command: %s\n\n", command)
        print_usage()
        os.exit(1)
    }
}

print_usage :: proc() {
    fmt.eprintln("Usage: sprite-tool <command> [args...]")
    fmt.eprintln("")
    fmt.eprintln("Commands:")
    fmt.eprintln("  launch    Create and configure a sprite with coding agent, git, beads")
    fmt.eprintln("  push      Push local file or directory to a sprite")
    fmt.eprintln("  pull      Pull file or directory from a sprite")
    fmt.eprintln("  watch     Poll a sprite's beads tracker task for progress")
    fmt.eprintln("")
    fmt.eprintln("Run 'sprite-tool <command> --help' for more information on a command.")
}
