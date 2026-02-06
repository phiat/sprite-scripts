## pull.nim - sprite-tool pull subcommand
## Pull a file or directory from a sprite to the local filesystem.

import os, osproc, strutils
import sprite

const pullUsage* = """
Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

Examples:
  sprite-tool pull /home/sprite/file.txt ./file.txt
  sprite-tool pull /home/sprite/mydir ./mydir
  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk
"""

proc runPull*(args: seq[string]) =
  if args.len < 2:
    echo pullUsage
    quit(1)

  let remotePath = args[0]
  let localPath = args[1]
  let spriteName = if args.len > 2: args[2] else: ""

  # Build sprite args string piece
  var spriteArgs = ""
  if spriteName != "":
    spriteArgs = " -s " & quoteShell(spriteName)

  # Check if remote path is a directory
  let checkCmd = "sprite exec" & spriteArgs &
                 " bash -c " & quoteShell("[ -d " & quoteShell(remotePath) & " ] && echo 'dir' || echo 'file'")
  let (output, exitCode) = execCmdEx(checkCmd)
  if exitCode != 0:
    echo "ERROR: Failed to check remote path"
    quit(1)

  let isDir = output.strip() == "dir"

  if isDir:
    echo "Pulling directory: " & remotePath & " -> " & localPath
    createDir(localPath)
    let cmd = "sprite exec" & spriteArgs &
              " tar czf - -C " & quoteShell(remotePath) & " ." &
              " | tar xzf - -C " & quoteShell(localPath)
    let rc = execCmd(cmd)
    if rc != 0:
      echo "ERROR: Failed to pull directory"
      quit(1)
  else:
    echo "Pulling file: " & remotePath & " -> " & localPath
    let localDir = parentDir(localPath)
    if localDir != "":
      createDir(localDir)
    let cmd = "sprite exec" & spriteArgs &
              " cat " & quoteShell(remotePath) &
              " > " & quoteShell(localPath)
    let rc = execCmd(cmd)
    if rc != 0:
      echo "ERROR: Failed to pull file"
      quit(1)

  echo "Done."
