## push.nim - sprite-tool push subcommand
## Push a local file or directory to a sprite.

import os, osproc, strutils
import sprite

const pushUsage* = """
Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

Examples:
  sprite-tool push ./file.txt /home/sprite/file.txt
  sprite-tool push ./mydir /home/sprite/mydir
  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk
"""

proc runPush*(args: seq[string]) =
  if args.len < 2:
    echo pushUsage
    quit(1)

  let localPath = args[0]
  let remotePath = args[1]
  let spriteName = if args.len > 2: args[2] else: ""

  # Build sprite args string piece
  var spriteArgs = ""
  if spriteName != "":
    spriteArgs = " -s " & quoteShell(spriteName)

  if not fileExists(localPath) and not dirExists(localPath):
    stderr.writeLine("Error: " & localPath & " does not exist")
    quit(1)

  if dirExists(localPath):
    echo "Pushing directory: " & localPath & " -> " & remotePath
    let parentPath = parentDir(localPath)
    let baseName = lastPathPart(localPath)
    let cmd = "tar czf - -C " & quoteShell(parentPath) & " " & quoteShell(baseName) &
              " | sprite exec" & spriteArgs &
              " bash -c " & quoteShell("mkdir -p " & quoteShell(remotePath) &
              " && tar xzf - -C " & quoteShell(remotePath) & " --strip-components=1")
    let rc = execCmd(cmd)
    if rc != 0:
      echo "ERROR: Failed to push directory"
      quit(1)
  else:
    echo "Pushing file: " & localPath & " -> " & remotePath
    let lastSlash = remotePath.rfind('/')
    let remoteDir = if lastSlash > 0: remotePath[0 ..< lastSlash] else: "/"
    let cmd = "sprite exec" & spriteArgs &
              " bash -c " & quoteShell("mkdir -p " & quoteShell(remoteDir) &
              " && cat > " & quoteShell(remotePath)) &
              " < " & quoteShell(localPath)
    let rc = execCmd(cmd)
    if rc != 0:
      echo "ERROR: Failed to push file"
      quit(1)

  echo "Done."
