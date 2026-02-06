-- | sprite-tool watch: Poll a sprite's beads tracker task for progress.
module Watch (run) where

import Control.Exception  (catch, SomeException)
import Control.Monad      (when)
import Data.Char          (toLower)
import Data.List          (isInfixOf)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr, hFlush, stdout)
import System.Process     (readProcess)
import Control.Concurrent (threadDelay)

-- | Print usage and exit.
usage :: IO a
usage = do
    putStrLn $ unlines
        [ "Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]"
        , ""
        , "Arguments:"
        , "  sprite-name     Name of the sprite to watch"
        , "  task-id         Beads task ID to track (default: auto-detect first open critical task)"
        , "  poll-interval   Seconds between polls (default: 30)"
        , ""
        , "Examples:"
        , "  sprite-tool watch ember-red-hawk"
        , "  sprite-tool watch ember-red-hawk CRM-1"
        , "  sprite-tool watch ember-red-hawk CRM-1 60"
        ]
    exitFailure

-- | Run a bash command inside a sprite, capturing stdout. Suppresses stderr.
sxCapture :: String -> String -> IO String
sxCapture spriteName cmd = do
    result <- catch
        (readProcess "sprite" ["exec", "-s", spriteName, "bash", "-c", cmd] "")
        (\e -> let _ = e :: SomeException in return "")
    return (strip result)
  where
    strip = reverse . dropWhile (== '\n') . reverse

-- | Execute the watch subcommand.
run :: [String] -> IO ()
run args
    | null args = usage
    | otherwise = do
        let spriteName   = head args
            taskIdArg    = if length args > 1 then args !! 1 else ""
            pollIntervalStr = if length args > 2 then args !! 2 else "30"

        pollInterval <- case reads pollIntervalStr :: [(Int, String)] of
            [(n, "")] -> return n
            _         -> do
                hPutStrLn stderr $ "Error: invalid poll-interval '" ++ pollIntervalStr ++ "' (must be integer)"
                exitFailure

        -- Auto-detect tracker task if not specified
        taskId <- if null taskIdArg
            then autoDetectTask spriteName
            else return taskIdArg

        putStrLn $ "Watching sprite '" ++ spriteName ++ "' task '" ++ taskId ++ "' (every " ++ show pollInterval ++ "s)"
        putStrLn "Press Ctrl+C to stop"
        putStrLn ""

        pollLoop spriteName taskId pollInterval

-- | Auto-detect the task ID from the sprite's beads tracker.
autoDetectTask :: String -> IO String
autoDetectTask spriteName = do
    putStrLn "Detecting tracker task..."

    -- Try critical tasks first
    taskId <- sxCapture spriteName
        "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'"

    taskId' <- if null taskId
        then do
            putStrLn "No critical task found. Falling back to first open task..."
            sxCapture spriteName
                "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'"
        else return taskId

    when (null taskId') $ do
        putStrLn $ "ERROR: No beads tasks found on sprite '" ++ spriteName ++ "'"
        putStrLn $ "Specify a task ID manually: sprite-tool watch " ++ spriteName ++ " <task-id>"
        exitFailure

    putStrLn $ "Tracking task: " ++ taskId'
    return taskId'

-- | Main poll loop: clear screen, show task, check if done, sleep.
pollLoop :: String -> String -> Int -> IO ()
pollLoop spriteName taskId pollInterval = do
    -- Clear screen using ANSI escape codes
    putStr "\ESC[2J\ESC[H"
    hFlush stdout

    -- Header with timestamp
    timeStr <- sxCapture spriteName "date '+%H:%M:%S'" `catch`
        (\e -> let _ = e :: SomeException in return "??:??:??")
    -- Fall back to a simple local approach if sprite date fails
    putStrLn $ "=== sprite-watch: " ++ spriteName ++ " / " ++ taskId ++ " === " ++ timeStr ++ " ==="
    putStrLn ""

    -- Show task status
    taskOutput <- sxCapture spriteName $
        "cd /home/sprite && bd show " ++ taskId ++ " 2>/dev/null"
    if null taskOutput
        then putStrLn "(could not read task)"
        else putStrLn taskOutput
    putStrLn ""

    -- Show recent comments
    putStrLn "--- Recent updates ---"
    commentsOutput <- sxCapture spriteName $
        "cd /home/sprite && bd comments " ++ taskId ++ " 2>/dev/null | tail -8"
    if null commentsOutput
        then putStrLn "(no comments)"
        else putStrLn commentsOutput
    putStrLn ""

    -- Check if done
    statusOutput <- sxCapture spriteName $
        "cd /home/sprite && bd show " ++ taskId ++ " 2>/dev/null | grep -i status"
    let statusLower = map toLower statusOutput
        isDone = any (`isInfixOf` statusLower) ["closed", "done", "completed"]

    if isDone
        then do
            putStrLn "=========================================="
            putStrLn "PROJECT COMPLETE"
            putStrLn "=========================================="
        else do
            threadDelay (pollInterval * 1000000)
            pollLoop spriteName taskId pollInterval
