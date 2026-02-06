-- | sprite-tool pull: Pull file or directory from a sprite.
module Pull (run) where

import System.Directory (createDirectoryIfMissing)
import System.Exit      (exitFailure, ExitCode(..))
import System.FilePath  (takeDirectory)
import System.IO        (hPutStrLn, stderr, hGetContents, hPutStr, hFlush, hClose)
import System.Process   ( createProcess, proc, readProcess, waitForProcess
                        , CreateProcess(..), StdStream(..) )

-- | Print usage and exit.
usage :: IO a
usage = do
    putStrLn $ unlines
        [ "Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]"
        , ""
        , "Examples:"
        , "  sprite-tool pull /home/sprite/file.txt ./file.txt"
        , "  sprite-tool pull /home/sprite/mydir ./mydir"
        , "  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk"
        ]
    exitFailure

-- | Execute the pull subcommand.
run :: [String] -> IO ()
run args
    | length args < 2 = usage
    | otherwise = do
        let remotePath = args !! 0
            localPath  = args !! 1
            spriteName = if length args > 2 then args !! 2 else ""
            sArgs      = if null spriteName then [] else ["-s", spriteName]

        -- Check if remote path is a directory or file
        let checkCmd = "[ -d '" ++ remotePath ++ "' ] && echo dir || echo file"
            spriteExecArgs = ["exec"] ++ sArgs ++ ["bash", "-c", checkCmd]
        output <- readProcess "sprite" spriteExecArgs ""
        let isDir = strip output == "dir"

        if isDir
            then pullDirectory remotePath localPath sArgs
            else pullSingleFile remotePath localPath sArgs

        putStrLn "Done."

-- | Pull a directory via tar pipe.
pullDirectory :: String -> String -> [String] -> IO ()
pullDirectory remotePath localPath sArgs = do
    putStrLn $ "Pulling directory: " ++ remotePath ++ " -> " ++ localPath
    createDirectoryIfMissing True localPath

    -- sprite exec ... tar czf - -C REMOTE . | tar xzf - -C LOCAL
    let spriteCmdArgs = ["exec"] ++ sArgs ++ ["tar", "czf", "-", "-C", remotePath, "."]
    (_, Just spriteOut, _, spritePh) <- createProcess
        (proc "sprite" spriteCmdArgs)
        { std_out = CreatePipe }

    (Just tarIn, _, _, tarPh) <- createProcess
        (proc "tar" ["xzf", "-", "-C", localPath])
        { std_in = CreatePipe }

    -- Pipe sprite output to local tar
    spriteContents <- hGetContents spriteOut
    hPutStr tarIn spriteContents
    hFlush tarIn
    hClose tarIn

    tarEc <- waitForProcess tarPh
    _ <- waitForProcess spritePh

    case tarEc of
        ExitSuccess   -> return ()
        ExitFailure _ -> do
            hPutStrLn stderr "Error: pull failed"
            exitFailure

-- | Pull a single file.
pullSingleFile :: String -> String -> [String] -> IO ()
pullSingleFile remotePath localPath sArgs = do
    putStrLn $ "Pulling file: " ++ remotePath ++ " -> " ++ localPath
    createDirectoryIfMissing True (takeDirectory localPath)

    -- sprite exec ... cat REMOTE > local
    let spriteCmdArgs = ["exec"] ++ sArgs ++ ["cat", remotePath]
    (_, Just spriteOut, _, spritePh) <- createProcess
        (proc "sprite" spriteCmdArgs)
        { std_out = CreatePipe }

    contents <- hGetContents spriteOut
    writeFile localPath contents

    ec <- waitForProcess spritePh
    case ec of
        ExitSuccess   -> return ()
        ExitFailure _ -> do
            hPutStrLn stderr "Error: pull failed"
            exitFailure

-- | Strip trailing newlines.
strip :: String -> String
strip = reverse . dropWhile (== '\n') . reverse
