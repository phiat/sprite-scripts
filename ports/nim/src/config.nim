## config.nim - Environment configuration and .env file loading

import os, strutils, tables

type
  Config* = object
    spriteToken*: string
    agent*: string           # "opencode" or "claude"
    claudeAuth*: string      # "subscription" or "apikey"
    anthropicApiKey*: string
    model*: string
    checkpointInterval*: int # seconds
    envFile*: string

proc loadEnvFile*(path: string): Table[string, string] =
  ## Parse a .env file into key=value pairs.
  ## Handles quoting, comments, and export prefixes.
  result = initTable[string, string]()
  if not fileExists(path):
    return
  for rawLine in lines(path):
    let line = rawLine.strip()
    # Skip empty lines and comments
    if line.len == 0 or line.startsWith("#"):
      continue
    # Strip optional "export " prefix
    var content = line
    if content.startsWith("export "):
      content = content[7..^1].strip()
    let eqPos = content.find('=')
    if eqPos < 0:
      continue
    let key = content[0 ..< eqPos].strip()
    var val = content[eqPos + 1 ..^ 1].strip()
    # Remove surrounding quotes
    if val.len >= 2:
      if (val[0] == '"' and val[^1] == '"') or
         (val[0] == '\'' and val[^1] == '\''):
        val = val[1 ..^ 2]
    result[key] = val

proc loadConfig*(): Config =
  ## Build a Config from environment variables and optional .env file.
  let envFile = getEnv("ENV_FILE", "./.env")

  # Load .env file first so its values can be read as fallbacks
  let envVars = loadEnvFile(envFile)

  # Apply .env values to the process environment so downstream code sees them
  for key, val in envVars:
    if getEnv(key) == "":
      putEnv(key, val)

  # SPRITE_TOKEN: prefer env, fall back to SPRITES_TOKEN from .env
  var spriteToken = getEnv("SPRITE_TOKEN")
  if spriteToken == "":
    spriteToken = getEnv("SPRITES_TOKEN")
  if spriteToken == "" and "SPRITES_TOKEN" in envVars:
    spriteToken = envVars["SPRITES_TOKEN"]

  let intervalStr = getEnv("CHECKPOINT_INTERVAL", "300")
  var interval = 300
  try:
    interval = parseInt(intervalStr)
  except ValueError:
    discard

  result = Config(
    spriteToken: spriteToken,
    agent: getEnv("AGENT", "opencode"),
    claudeAuth: getEnv("CLAUDE_AUTH", "subscription"),
    anthropicApiKey: getEnv("ANTHROPIC_API_KEY"),
    model: getEnv("MODEL"),
    checkpointInterval: interval,
    envFile: envFile,
  )
