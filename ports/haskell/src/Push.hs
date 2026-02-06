-- | sprite-tool push: Push local file or directory to a sprite.
module Push (run) where

import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit      (exitFailure, ExitCode(..))
import System.FilePath  (takeDirectory)
import System.IO        (hPutStrLn, stderr, hGetContents, hPutStr, hFlush, hClose)
import System.Process   ( createProcess, proc, waitForProcess
                        , CreateProcess(..), StdStream(..) )

-- | Print usage and exit.
usage :: IO a
usage = do
    putStrLn $ unlines
        [ "Usage: sprite-tool push <local-path> <remote-path> [sprite-name]"
        , ""
        , "Examples:"
        , "  sprite-tool push ./file.txt /home/sprite/file.txt"
        , "  sprite-tool push ./mydir /home/sprite/mydir"
        , "  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk"
        ]
    exitFailure

-- | Execute the push subcommand.
run :: [String] -> IO ()
run args
    | length args < 2 = usage
    | otherwise = do
        let localPath  = args !! 0
            remotePath = args !! 1
            spriteName = if length args > 2 then args !! 2 else ""
            sArgs      = if null spriteName then [] else ["-s", spriteName]

        -- Check local path exists
        fileExists <- doesFileExist localPath
        dirExists  <- doesDirectoryExist localPath
        if not fileExists && not dirExists
            then do
                hPutStrLn stderr $ "Error: " ++ localPath ++ " does not exist"
                exitFailure
            else return ()

        if dirExists
            then pushDirectory localPath remotePath sArgs
            else pushSingleFile localPath remotePath sArgs

        putStrLn "Done."

-- | Push a directory via tar pipe.
pushDirectory :: String -> String -> [String] -> IO ()
pushDirectory localPath remotePath sArgs = do
    putStrLn $ "Pushing directory: " ++ localPath ++ " -> " ++ remotePath
    let parent = takeDirectory localPath
        base   = takeBaseName' localPath

    -- tar czf - -C parent base | sprite exec ... bash -c "mkdir -p DEST && tar xzf - -C DEST --strip-components=1"
    (_, Just tarOut, _, tarPh) <- createProcess
        (proc "tar" ["czf", "-", "-C", parent, base])
        { std_out = CreatePipe }

    let spriteCmd = ["sprite", "exec"] ++ sArgs ++
            ["bash", "-c", "mkdir -p '" ++ remotePath ++ "' && tar xzf - -C '" ++ remotePath ++ "' --strip-components=1"]
    (Just spriteIn, _, _, spritePh) <- createProcess
        (proc (head spriteCmd) (tail spriteCmd))
        { std_in = CreatePipe }

    -- Pipe tar output to sprite stdin
    tarContents <- hGetContents tarOut
    hPutStr spriteIn tarContents
    hFlush spriteIn
    hClose spriteIn

    spriteEc <- waitForProcess spritePh
    _ <- waitForProcess tarPh

    case spriteEc of
        ExitSuccess   -> return ()
        ExitFailure _ -> do
            hPutStrLn stderr "Error: push failed"
            exitFailure

-- | Push a single file via stdin pipe.
pushSingleFile :: String -> String -> [String] -> IO ()
pushSingleFile localPath remotePath sArgs = do
    putStrLn $ "Pushing file: " ++ localPath ++ " -> " ++ remotePath
    let remoteDir = takeDirectory remotePath

    let spriteCmd = ["sprite", "exec"] ++ sArgs ++
            ["bash", "-c", "mkdir -p '" ++ remoteDir ++ "' && cat > '" ++ remotePath ++ "'"]

    fileContents <- readFile localPath
    (Just hin, _, _, ph) <- createProcess
        (proc (head spriteCmd) (tail spriteCmd))
        { std_in = CreatePipe }

    hPutStr hin fileContents
    hFlush hin
    hClose hin

    ec <- waitForProcess ph
    case ec of
        ExitSuccess   -> return ()
        ExitFailure _ -> do
            hPutStrLn stderr "Error: push failed"
            exitFailure

-- | Extract directory basename (last path component, preserving extension).
takeBaseName' :: String -> String
takeBaseName' p =
    let stripped = if not (null p) && last p == '/' then init p else p
    in reverse $ takeWhile (/= '/') (reverse stripped)
