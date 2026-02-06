-- | sprite-tool launch: Create and configure a sprite with coding agent, git, beads.
module Launch (run) where

import Control.Concurrent    (forkIO, threadDelay, MVar, tryTakeMVar, putMVar, newEmptyMVar)
import Control.Exception     (SomeException, try)
import Control.Monad         (when, unless, forM_)
import Data.List             (isPrefixOf)
import System.Directory      (doesFileExist, doesDirectoryExist, findExecutable, getHomeDirectory)
import System.Environment    (setEnv, lookupEnv)
import System.Exit           (exitFailure)
import System.FilePath        ((</>))
import System.IO             (hPutStrLn, stderr, hFlush, stdout)
import System.Process        (callProcess)

import Config
import Sprite

-- | Print usage and exit.
usage :: IO a
usage = do
    putStrLn $ unlines
        [ "Usage: sprite-tool launch [options] <sprite-name> [plan-file]"
        , ""
        , "Options:"
        , "  --dry-run              Show what would happen without executing"
        , "  --no-checkpoint        Disable auto-checkpointing"
        , "  --upload <dir>         Upload a local directory to /home/sprite/<dirname>"
        , "                         (repeatable: --upload ./data --upload ./tests)"
        , ""
        , "Environment variables:"
        , "  ENV_FILE               Path to .env file (default: ./.env)"
        , "  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)"
        , "  AGENT                  \"opencode\" (default) or \"claude\""
        , "  CLAUDE_AUTH            \"subscription\" (default) or \"apikey\""
        , "  MODEL                  Model override (see below)"
        , "  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)"
        , ""
        , "Model examples:"
        , "  OpenCode: MODEL=opencode/big-pickle  (free, default)"
        , "            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)"
        , "            MODEL=openai/gpt-4o"
        , "            MODEL=google/gemini-2.5-pro"
        , "  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku"
        , ""
        , "Examples:"
        , "  sprite-tool launch my-project plan.md"
        , "  sprite-tool launch --upload ./data my-project plan.md"
        , "  sprite-tool launch --upload ./data --upload ./tests my-project plan.md"
        , "  sprite-tool launch --dry-run my-project plan.md"
        , "  MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md"
        , "  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md"
        ]
    exitFailure

-- | Parse CLI flags for the launch subcommand.
--   Returns (Config with flags applied, spriteName, planFile).
parseArgs :: Config -> [String] -> IO (Config, String, String)
parseArgs cfg0 args0 = go cfg0 [] args0
  where
    go cfg positional [] =
        case positional of
            []     -> usage
            [s]    -> return (cfg, s, "")
            (s:p:_) -> return (cfg, s, p)
    go cfg positional ("--dry-run"       : rest) = go cfg { cfgDryRun = True } positional rest
    go cfg positional ("--no-checkpoint" : rest) = go cfg { cfgCheckpointing = False } positional rest
    go cfg positional ("--upload" : dir  : rest) =
        go cfg { cfgUploadDirs = cfgUploadDirs cfg ++ [dir] } positional rest
    go _   _          ("--upload" : []) = do
        hPutStrLn stderr "Error: --upload requires an argument"
        exitFailure
    go _   _          ("--help" : _) = usage
    go _   _          ("-h"     : _) = usage
    go _   _          (opt : _)
        | "--" `isPrefixOf` opt = do
            putStrLn $ "Unknown option: " ++ opt
            usage
    go cfg positional (arg : rest) = go cfg (positional ++ [arg]) rest

-- | Background checkpoint loop. Writes to the stop MVar to signal termination.
--   The threadId is returned so the caller can manage cleanup.
data CheckpointLoop = CheckpointLoop
    { clStopVar :: MVar ()     -- put () to signal stop
    , clDoneVar :: MVar ()     -- take to wait for thread to finish
    }

startCheckpointing :: String -> Int -> IO CheckpointLoop
startCheckpointing spriteName interval = do
    stopVar <- newEmptyMVar
    doneVar <- newEmptyMVar
    putStrLn $ "Auto-checkpointing every " ++ show interval ++ "s (thread)"
    _ <- forkIO $ checkpointLoop stopVar doneVar
    return CheckpointLoop { clStopVar = stopVar, clDoneVar = doneVar }
  where
    checkpointLoop stopVar doneVar = do
        let delayUs = interval * 1000000
        loop delayUs stopVar
        putMVar doneVar ()

    loop delayUs stopVar = do
        -- Sleep in 1-second increments so we can check the stop signal
        stopped <- sleepOrStop delayUs stopVar
        unless stopped $ do
            putStrLn "[checkpoint] Creating checkpoint..."
            hFlush stdout
            result <- try (callProcess "sprite"
                ["checkpoint", "create", "-s", spriteName]) :: IO (Either SomeException ())
            case result of
                Right () -> putStrLn "[checkpoint] Done."
                Left _   -> putStrLn "[checkpoint] Failed (non-fatal)."
            hFlush stdout
            loop delayUs stopVar

    -- Sleep for the given microseconds, checking stopVar periodically.
    -- Returns True if stop was signaled.
    sleepOrStop :: Int -> MVar () -> IO Bool
    sleepOrStop totalUs stopVar = go' 0
      where
        chunkUs = 500000  -- check every 0.5 seconds
        go' elapsed
            | elapsed >= totalUs = return False
            | otherwise = do
                threadDelay (min chunkUs (totalUs - elapsed))
                stopped <- tryTakeMVar stopVar
                case stopped of
                    Just _  -> return True
                    Nothing -> go' (elapsed + chunkUs)

stopCheckpointing :: CheckpointLoop -> IO ()
stopCheckpointing cl = do
    putMVar (clStopVar cl) ()
    -- Wait for the thread to finish (with a timeout via threadDelay if needed)
    _ <- tryTakeMVar (clDoneVar cl)
    -- Give it a moment to exit
    threadDelay 100000
    _ <- tryTakeMVar (clDoneVar cl)
    return ()

-- | Execute the launch subcommand.
run :: [String] -> IO ()
run args = do
    -- Load config from .env + environment
    cfg0 <- loadConfig

    -- Parse flags
    (cfg, spriteName, planFile) <- parseArgs cfg0 args

    let dryRun = cfgDryRun cfg

    -- Shortcuts
    let sx'        cmd = Sprite.sx spriteName cmd dryRun
        sxPass'    cmd = Sprite.sxPassthrough spriteName cmd dryRun
        pushFile'  s d = Sprite.pushFile spriteName s d dryRun
        pushDir'   s d = Sprite.pushDir spriteName s d dryRun

    -- ----------------------------------------------------------------
    -- 1. Check/install sprite CLI
    -- ----------------------------------------------------------------
    spriteFound <- findExecutable "sprite"
    case spriteFound of
        Just _  -> return ()
        Nothing ->
            if dryRun
                then putStrLn "  [dry-run] Would install sprite CLI"
                else do
                    putStrLn "Installing sprite CLI..."
                    callProcess "bash" ["-c", "curl -fsSL https://sprites.dev/install.sh | sh"]
                    home <- getHomeDirectory
                    existingPath <- lookupEnv "PATH"
                    let localBin = home </> ".local" </> "bin"
                        newPath = localBin ++ ":" ++ maybe "" id existingPath
                    setEnv "PATH" newPath

    -- ----------------------------------------------------------------
    -- 2. Auth sprite (non-interactive if token provided)
    -- ----------------------------------------------------------------
    if not (null (cfgSpriteToken cfg))
        then do
            putStrLn "Authenticating sprite with token..."
            unless dryRun $
                callProcess "sprite" ["auth", "setup", "--token", cfgSpriteToken cfg]
        else do
            putStrLn "No SPRITE_TOKEN set. Running interactive login..."
            unless dryRun $
                callProcess "sprite" ["login"]

    -- ----------------------------------------------------------------
    -- 3. Create sprite (or use existing)
    -- ----------------------------------------------------------------
    if dryRun
        then putStrLn $ "  [dry-run] Would create or reuse sprite '" ++ spriteName ++ "'"
        else do
            exists <- spriteExists spriteName
            if exists
                then putStrLn $ "Sprite '" ++ spriteName ++ "' already exists, using it."
                else do
                    putStrLn $ "Creating sprite: " ++ spriteName
                    callProcess "sprite" ["create", "-skip-console", spriteName]

    -- ----------------------------------------------------------------
    -- 4. Push .env to sprite
    -- ----------------------------------------------------------------
    envExists <- doesFileExist (cfgEnvFile cfg)
    when envExists $ do
        putStrLn $ "Pushing " ++ cfgEnvFile cfg ++ "..."
        pushFile' (cfgEnvFile cfg) "/home/sprite/.env"

    -- ----------------------------------------------------------------
    -- 5. Push plan file if provided
    -- ----------------------------------------------------------------
    when (not (null planFile)) $ do
        planExists <- doesFileExist planFile
        when planExists $ do
            putStrLn $ "Pushing " ++ planFile ++ "..."
            pushFile' planFile "/home/sprite/plan.md"

    -- ----------------------------------------------------------------
    -- 6. Upload directories if provided
    -- ----------------------------------------------------------------
    forM_ (cfgUploadDirs cfg) $ \dir -> do
        isDir <- doesDirectoryExist dir
        if isDir
            then do
                let dirname = takeBaseName' dir
                putStrLn $ "Uploading directory: " ++ dir ++ " -> /home/sprite/" ++ dirname
                pushDir' dir ("/home/sprite/" ++ dirname)
            else
                putStrLn $ "WARNING: --upload dir '" ++ dir ++ "' not found, skipping."

    -- ----------------------------------------------------------------
    -- 7. Setup git + beads
    -- ----------------------------------------------------------------
    putStrLn "Initializing git..."
    sx' "cd /home/sprite && git init -b main 2>/dev/null || true"

    putStrLn "Installing beads..."
    sx' "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash"

    -- ----------------------------------------------------------------
    -- 8. Install and auth coding agent
    -- ----------------------------------------------------------------
    case cfgAgent cfg of
        "claude" -> do
            putStrLn "Setting up claude..."
            sx' "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code"

            case cfgClaudeAuth cfg of
                "subscription" -> do
                    home <- getHomeDirectory
                    let credsPath = home </> ".claude" </> ".credentials.json"
                    credsExist <- doesFileExist credsPath
                    if credsExist
                        then do
                            putStrLn "Copying claude subscription credentials..."
                            pushFile' credsPath "/home/sprite/.claude/.credentials.json"
                            sx' "chmod 600 ~/.claude/.credentials.json"
                        else do
                            putStrLn "ERROR: ~/.claude/.credentials.json not found"
                            putStrLn "Run 'claude' locally first to authenticate, then re-run this script."
                            exitFailure

                "apikey"
                    | not (null (cfgAnthropicApiKey cfg)) -> do
                        putStrLn "Setting ANTHROPIC_API_KEY in sprite..."
                        sx' $ "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || "
                           ++ "echo 'export ANTHROPIC_API_KEY=\"" ++ cfgAnthropicApiKey cfg ++ "\"' >> ~/.bashrc"

                _ -> do
                    putStrLn "ERROR: No valid claude auth configured"
                    putStrLn "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY"
                    exitFailure

        "opencode" -> do
            putStrLn "Setting up opencode..."
            sx' "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash"
            sx' $ "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || "
               ++ "echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc"

        other -> do
            putStrLn $ "ERROR: Unknown AGENT '" ++ other ++ "'. Use 'claude' or 'opencode'."
            exitFailure

    -- ----------------------------------------------------------------
    -- 9. Launch agent with plan (or open console)
    -- ----------------------------------------------------------------
    putStrLn ""
    putStrLn "=========================================="
    putStrLn $ "Sprite '" ++ spriteName ++ "' is ready!"
    let modelNote = if null (cfgModel cfg)
            then ""
            else " (model: " ++ cfgModel cfg ++ ")"
    putStrLn $ "Agent: " ++ cfgAgent cfg ++ modelNote
    when (cfgCheckpointing cfg) $
        putStrLn $ "Checkpointing: every " ++ show (cfgCheckpointInterval cfg) ++ "s"
    putStrLn "=========================================="

    if dryRun
        then do
            putStrLn ""
            putStrLn $ "[dry-run] Would launch " ++ cfgAgent cfg ++ " with plan. No changes were made."
        else if not (null planFile)
            then do
                -- Start auto-checkpointing before agent runs
                mCl <- if cfgCheckpointing cfg
                    then do
                        cl <- startCheckpointing spriteName (cfgCheckpointInterval cfg)
                        return (Just cl)
                    else return Nothing

                putStrLn $ "Launching " ++ cfgAgent cfg ++ " with plan..."

                case cfgAgent cfg of
                    "claude" -> do
                        let modelFlag = if null (cfgModel cfg) then "" else "--model " ++ cfgModel cfg ++ " "
                        _ <- sxPass' $ "cd /home/sprite && claude " ++ modelFlag
                                    ++ "-p 'read plan.md and complete the plan please'"
                        return ()

                    "opencode" -> do
                        let ocModel = if null (cfgModel cfg) then "opencode/big-pickle" else cfgModel cfg
                        _ <- sxPass' $ "set -a && source /home/sprite/.env 2>/dev/null && set +a && "
                                    ++ "cd /home/sprite && ~/.opencode/bin/opencode run -m " ++ ocModel
                                    ++ " 'read plan.md and complete the plan please'"
                        return ()

                    _ -> return ()

                -- Stop checkpointing
                case mCl of
                    Just cl -> stopCheckpointing cl
                    Nothing -> return ()

                -- Final checkpoint
                putStrLn "Creating final checkpoint..."
                result <- try (callProcess "sprite"
                    ["checkpoint", "create", "-s", spriteName]) :: IO (Either SomeException ())
                case result of
                    Right () -> putStrLn "Final checkpoint saved."
                    Left _   -> putStrLn "Final checkpoint failed (non-fatal)."

            else do
                putStrLn "Opening console..."
                callProcess "sprite" ["console", "-s", spriteName]
  where
    -- Extract directory basename (last path component, not stripping extension).
    takeBaseName' :: String -> String
    takeBaseName' p =
        let stripped = if not (null p) && last p == '/' then init p else p
        in reverse $ takeWhile (/= '/') (reverse stripped)
