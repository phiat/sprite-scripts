-- | sprite-tool: Haskell port of sprite-scripts.
--   Entry point and subcommand dispatch.
module Main (main) where

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

import qualified Launch
import qualified Push
import qualified Pull
import qualified Watch

-- | Print top-level usage and exit.
usage :: IO a
usage = do
    putStrLn $ unlines
        [ "sprite-tool: manage sprites with coding agents"
        , ""
        , "Usage: sprite-tool <command> [args...]"
        , ""
        , "Commands:"
        , "  launch    Create and configure a sprite with coding agent, git, beads"
        , "  push      Push local file or directory to a sprite"
        , "  pull      Pull file or directory from a sprite"
        , "  watch     Poll a sprite's beads tracker task for progress"
        , ""
        , "Run 'sprite-tool <command> --help' for command-specific help."
        ]
    exitFailure

-- | Main entry point: dispatch to subcommand.
main :: IO ()
main = do
    args <- getArgs
    case args of
        []               -> usage
        ("launch" : rest) -> Launch.run rest
        ("push"   : rest) -> Push.run rest
        ("pull"   : rest) -> Pull.run rest
        ("watch"  : rest) -> Watch.run rest
        ("--help" : _)    -> usage
        ("-h"     : _)    -> usage
        (cmd      : _)    -> do
            hPutStrLn stderr $ "Unknown command: " ++ cmd
            usage
