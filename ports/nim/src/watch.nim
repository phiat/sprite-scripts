## watch.nim - sprite-tool watch subcommand
## Poll a sprite's beads tracker task for progress.

import os, osproc, strutils, times
import sprite

const watchUsage* = """
Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]

Arguments:
  sprite-name     Name of the sprite to watch
  task-id         Beads task ID to track (default: auto-detect first open critical task)
  poll-interval   Seconds between polls (default: 30)

Examples:
  sprite-tool watch ember-red-hawk
  sprite-tool watch ember-red-hawk CRM-1
  sprite-tool watch ember-red-hawk CRM-1 60
"""

proc runWatch*(args: seq[string]) =
  if args.len < 1:
    echo watchUsage
    quit(1)

  let spriteName = args[0]
  var taskId = if args.len > 1: args[1] else: ""
  var pollInterval = 30
  if args.len > 2:
    try:
      pollInterval = parseInt(args[2])
    except ValueError:
      echo "ERROR: Invalid poll interval: " & args[2]
      quit(1)

  # Auto-detect tracker task if not specified
  if taskId == "":
    echo "Detecting tracker task..."
    let (critOut, critRc) = sxCapture(spriteName,
      "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'")
    if critRc == 0 and critOut.strip() != "":
      taskId = critOut.strip()
    else:
      echo "No critical task found. Falling back to first open task..."
      let (openOut, openRc) = sxCapture(spriteName,
        "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'")
      if openRc == 0 and openOut.strip() != "":
        taskId = openOut.strip()

    if taskId == "":
      echo "ERROR: No beads tasks found on sprite '" & spriteName & "'"
      echo "Specify a task ID manually: sprite-tool watch " & spriteName & " <task-id>"
      quit(1)

    echo "Tracking task: " & taskId

  echo "Watching sprite '" & spriteName & "' task '" & taskId & "' (every " & $pollInterval & "s)"
  echo "Press Ctrl+C to stop"
  echo ""

  while true:
    # Clear screen
    discard execCmd("clear")

    let timeStr = now().format("HH:mm:ss")
    echo "=== sprite-watch: " & spriteName & " / " & taskId & " === " & timeStr & " ==="
    echo ""

    # Show task status
    let showRc = sx(spriteName, "cd /home/sprite && bd show " & taskId & " 2>/dev/null")
    if showRc != 0:
      echo "(could not read task)"
    echo ""

    # Show recent comments
    echo "--- Recent updates ---"
    let commentsRc = sx(spriteName, "cd /home/sprite && bd comments " & taskId & " 2>/dev/null | tail -8")
    if commentsRc != 0:
      echo "(no comments)"
    echo ""

    # Check if done
    let (statusOut, _) = sxCapture(spriteName,
      "cd /home/sprite && bd show " & taskId & " 2>/dev/null | grep -i status")
    let statusLower = statusOut.toLowerAscii()
    if statusLower.contains("closed") or statusLower.contains("done") or statusLower.contains("completed"):
      echo "=========================================="
      echo "PROJECT COMPLETE"
      echo "=========================================="
      break

    sleep(pollInterval * 1000)
