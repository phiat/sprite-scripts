## sprite.nim - Wrapper around the `sprite` CLI binary

import os, osproc, strutils

proc spriteExec*(spriteName: string, args: varargs[string]): int =
  ## Run `sprite exec -s <name> <args...>` and return the exit code.
  ## Stdout/stderr pass through to the terminal.
  var cmd = "sprite exec"
  if spriteName != "":
    cmd &= " -s " & quoteShell(spriteName)
  for a in args:
    cmd &= " " & quoteShell(a)
  result = execCmd(cmd)

proc spriteExecCapture*(spriteName: string, args: varargs[string]): tuple[output: string, exitCode: int] =
  ## Run `sprite exec -s <name> <args...>` and capture output.
  var cmd = "sprite exec"
  if spriteName != "":
    cmd &= " -s " & quoteShell(spriteName)
  for a in args:
    cmd &= " " & quoteShell(a)
  result = execCmdEx(cmd)

proc sx*(spriteName: string, bashCmd: string, dryRun: bool = false): int =
  ## Execute a bash command inside the sprite. Returns exit code.
  if dryRun:
    echo "  [dry-run] sprite exec -s " & spriteName & " bash -c " & quoteShell(bashCmd)
    return 0
  return spriteExec(spriteName, "bash", "-c", bashCmd)

proc sxCapture*(spriteName: string, bashCmd: string): tuple[output: string, exitCode: int] =
  ## Execute a bash command inside the sprite and capture output.
  var cmd = "sprite exec"
  if spriteName != "":
    cmd &= " -s " & quoteShell(spriteName)
  cmd &= " bash -c " & quoteShell(bashCmd)
  result = execCmdEx(cmd)

proc spriteCmd*(args: varargs[string]): int =
  ## Run a bare `sprite <args...>` command and return exit code.
  var cmd = "sprite"
  for a in args:
    cmd &= " " & quoteShell(a)
  result = execCmd(cmd)

proc spriteCmdCapture*(args: varargs[string]): tuple[output: string, exitCode: int] =
  ## Run a bare `sprite <args...>` command and capture output.
  var cmd = "sprite"
  for a in args:
    cmd &= " " & quoteShell(a)
  result = execCmdEx(cmd)

proc pushFile*(spriteName: string, localPath: string, remotePath: string, dryRun: bool = false): int =
  ## Push a local file to the sprite at remotePath.
  if dryRun:
    echo "  [dry-run] push " & localPath & " -> sprite:" & remotePath
    return 0
  let lastSlash = remotePath.rfind('/')
  let remoteDir = if lastSlash > 0: remotePath[0 ..< lastSlash] else: "/"
  discard sx(spriteName, "mkdir -p " & quoteShell(remoteDir))
  # Pipe file content via stdin to cat > remotePath
  let cmd = "sprite exec -s " & quoteShell(spriteName) &
            " bash -c " & quoteShell("cat > " & quoteShell(remotePath)) &
            " < " & quoteShell(localPath)
  result = execCmd(cmd)

proc pushDir*(spriteName: string, localPath: string, remotePath: string, dryRun: bool = false): int =
  ## Push a local directory to the sprite at remotePath using tar pipe.
  if dryRun:
    echo "  [dry-run] push dir " & localPath & " -> sprite:" & remotePath
    return 0
  discard sx(spriteName, "mkdir -p " & quoteShell(remotePath))
  let parentDir = parentDir(localPath)
  let baseName = lastPathPart(localPath)
  let cmd = "tar czf - -C " & quoteShell(parentDir) & " " & quoteShell(baseName) &
            " | sprite exec -s " & quoteShell(spriteName) &
            " bash -c " & quoteShell("tar xzf - -C " & quoteShell(remotePath) & " --strip-components=1")
  result = execCmd(cmd)

proc ensureSpriteCli*(dryRun: bool = false) =
  ## Check if `sprite` is on PATH; install if not.
  if findExe("sprite") != "":
    return
  if dryRun:
    echo "  [dry-run] Would install sprite CLI"
    return
  echo "Installing sprite CLI..."
  let rc = execCmd("curl -fsSL https://sprites.dev/install.sh | sh")
  if rc != 0:
    echo "ERROR: Failed to install sprite CLI"
    quit(1)
  putEnv("PATH", getHomeDir() / ".local" / "bin" & ":" & getEnv("PATH"))

proc authSprite*(token: string, dryRun: bool = false) =
  ## Authenticate with sprites.dev. Uses token if available, else interactive login.
  if token != "":
    echo "Authenticating sprite with token..."
    if not dryRun:
      let rc = spriteCmd("auth", "setup", "--token", token)
      if rc != 0:
        echo "ERROR: sprite auth failed"
        quit(1)
  else:
    echo "No SPRITE_TOKEN set. Running interactive login..."
    if not dryRun:
      let rc = spriteCmd("login")
      if rc != 0:
        echo "ERROR: sprite login failed"
        quit(1)

proc spriteExists*(name: string): bool =
  ## Check if a sprite with the given name already exists.
  let (output, exitCode) = spriteCmdCapture("ls")
  if exitCode != 0:
    return false
  for line in output.splitLines():
    if line.strip().contains(name):
      # Check word boundary - the name should appear as a distinct token
      for word in line.split():
        if word.strip() == name:
          return true
  return false
