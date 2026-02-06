-- | Subprocess wrapper for the sprite CLI.
module Sprite
    ( sx
    , sxCapture
    , sxPassthrough
    , pushFile
    , pushDir
    , spriteList
    , spriteExists
    , spriteArgs
    ) where

import System.Exit     (ExitCode(..))
import System.IO       (hClose, hFlush, hGetContents, hPutStr)
import System.Process  ( CreateProcess(..), StdStream(..), createProcess
                       , proc, waitForProcess, callProcess, readProcess )
import System.FilePath  (takeDirectory)

-- | Build the [-s, spriteName] args list, or empty if sprite name is empty.
spriteArgs :: String -> [String]
spriteArgs ""   = []
spriteArgs name = ["-s", name]

-- | Run a bash command inside a sprite. Output goes to stdout/stderr of the
--   calling process (passthrough). Dry-run just prints. Returns exit code.
sxPassthrough :: String -> String -> Bool -> IO ExitCode
sxPassthrough spriteName cmd dryRun
    | dryRun = do
        putStrLn $ "  [dry-run] sprite exec -s " ++ spriteName ++ " bash -c \"" ++ cmd ++ "\""
        return ExitSuccess
    | otherwise = do
        (_, _, _, ph) <- createProcess (proc "sprite" args)
        waitForProcess ph
  where
    args = ["exec", "-s", spriteName, "bash", "-c", cmd]

-- | Run a bash command inside a sprite, capturing stdout. Dry-run just prints.
sxCapture :: String -> String -> Bool -> IO String
sxCapture spriteName cmd dryRun
    | dryRun = do
        putStrLn $ "  [dry-run] sprite exec -s " ++ spriteName ++ " bash -c \"" ++ cmd ++ "\""
        return ""
    | otherwise = do
        output <- readProcess "sprite" args ""
        return (strip output)
  where
    args = ["exec", "-s", spriteName, "bash", "-c", cmd]
    strip = reverse . dropWhile (== '\n') . reverse

-- | Run a bash command inside a sprite. Convenience wrapper that discards
--   the exit code but still passes through output. For dry-run, prints the command.
sx :: String -> String -> Bool -> IO ()
sx spriteName cmd dryRun = do
    _ <- sxPassthrough spriteName cmd dryRun
    return ()

-- | Push a local file to a sprite.
--   Equivalent to:
--     sprite exec -s SPRITE bash -c "mkdir -p $(dirname DEST)"
--     sprite exec -s SPRITE bash -c "cat > DEST" < src
pushFile :: String -> String -> String -> Bool -> IO ()
pushFile spriteName src dest dryRun
    | dryRun = putStrLn $ "  [dry-run] push " ++ src ++ " -> sprite:" ++ dest
    | otherwise = do
        let destDir = takeDirectory dest
        -- Create destination directory
        callProcess "sprite"
            ["exec", "-s", spriteName, "bash", "-c", "mkdir -p '" ++ destDir ++ "'"]
        -- Read local file and pipe to cat on the sprite
        fileContents <- readFile src
        (Just hin, _, _, ph) <- createProcess
            (proc "sprite" ["exec", "-s", spriteName, "bash", "-c", "cat > '" ++ dest ++ "'"])
            { std_in = CreatePipe }
        hPutStr hin fileContents
        hFlush hin
        hClose hin
        _ <- waitForProcess ph
        return ()

-- | Push a local directory to a sprite via tar.
--   Equivalent to:
--     sprite exec -s SPRITE bash -c "mkdir -p DEST"
--     tar czf - -C parent base | sprite exec -s SPRITE bash -c "tar xzf - -C destParent"
pushDir :: String -> String -> String -> Bool -> IO ()
pushDir spriteName src dest dryRun
    | dryRun = putStrLn $ "  [dry-run] push dir " ++ src ++ " -> sprite:" ++ dest
    | otherwise = do
        let srcParent = takeDirectory src
            srcBase   = takeBaseName' src
            destParent = takeDirectory dest
        -- Create destination directory
        callProcess "sprite"
            ["exec", "-s", spriteName, "bash", "-c", "mkdir -p '" ++ dest ++ "'"]
        -- tar czf - -C parent base | sprite exec ... bash -c "tar xzf - -C destParent"
        (_, Just tarOut, _, tarPh) <- createProcess
            (proc "tar" ["czf", "-", "-C", srcParent, srcBase])
            { std_out = CreatePipe }
        (Just spriteIn, _, _, spritePh) <- createProcess
            (proc "sprite" [ "exec", "-s", spriteName, "bash", "-c"
                           , "tar xzf - -C '" ++ destParent ++ "'" ])
            { std_in = CreatePipe }
        -- Pipe tar output to sprite stdin
        tarContents <- hGetContents tarOut
        hPutStr spriteIn tarContents
        hFlush spriteIn
        hClose spriteIn
        _ <- waitForProcess spritePh
        _ <- waitForProcess tarPh
        return ()
  where
    -- Extract the basename from a path (last component).
    -- We avoid System.FilePath.takeBaseName because that strips extensions.
    takeBaseName' :: String -> String
    takeBaseName' p =
        let stripped = if not (null p) && last p == '/' then init p else p
        in reverse $ takeWhile (/= '/') (reverse stripped)

-- | Run 'sprite ls' and return stdout.
spriteList :: IO String
spriteList = readProcess "sprite" ["ls"] ""

-- | Check if a sprite with the given name already exists.
spriteExists :: String -> IO Bool
spriteExists spriteName = do
    output <- spriteList
    let lns = lines output
        -- Check for the sprite name as a whole word in each line
        hasName l = spriteName `elem` words l
    return $ any hasName lns
