## sprite_tool.nim - Main entry point and subcommand dispatch

import os
import launch, push, pull, watch

const mainUsage = """
sprite-tool: Sprite management tool

Usage: sprite-tool <command> [args...]

Commands:
  launch    Create and configure a sprite with a coding agent
  push      Push a local file or directory to a sprite
  pull      Pull a file or directory from a sprite
  watch     Watch a sprite's beads task for progress

Run 'sprite-tool <command> --help' for details on each command.
"""

proc main() =
  let args = commandLineParams()
  if args.len == 0:
    echo mainUsage
    quit(1)

  let command = args[0]
  let subArgs = args[1..^1]

  case command
  of "launch":
    runLaunch(subArgs)
  of "push":
    runPush(subArgs)
  of "pull":
    runPull(subArgs)
  of "watch":
    runWatch(subArgs)
  of "--help", "-h", "help":
    echo mainUsage
    quit(0)
  else:
    echo "Unknown command: " & command
    echo mainUsage
    quit(1)

when isMainModule:
  main()
