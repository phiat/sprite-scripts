-- | Configuration: .env parsing and environment variable loading.
module Config
    ( Config(..)
    , defaultConfig
    , loadEnvFile
    , loadConfig
    ) where

import Data.Char     (isSpace)
import Data.List     (isPrefixOf)
import System.IO     (hPutStrLn, stderr)
import System.Environment (lookupEnv, setEnv)
import System.Directory   (doesFileExist)
import System.Exit        (exitFailure)

-- | All configuration derived from environment variables and .env files.
data Config = Config
    { cfgSpriteToken        :: String
    , cfgAgent              :: String   -- "opencode" or "claude"
    , cfgClaudeAuth         :: String   -- "subscription" or "apikey"
    , cfgAnthropicApiKey    :: String
    , cfgModel              :: String
    , cfgCheckpointInterval :: Int      -- seconds (default 300)
    , cfgEnvFile            :: String
    -- CLI flags (populated by launch arg parsing)
    , cfgDryRun             :: Bool
    , cfgCheckpointing      :: Bool
    , cfgUploadDirs         :: [String]
    } deriving (Show)

-- | Sensible defaults matching the bash original.
defaultConfig :: Config
defaultConfig = Config
    { cfgSpriteToken        = ""
    , cfgAgent              = "opencode"
    , cfgClaudeAuth         = "subscription"
    , cfgAnthropicApiKey    = ""
    , cfgModel              = ""
    , cfgCheckpointInterval = 300
    , cfgEnvFile            = "./.env"
    , cfgDryRun             = False
    , cfgCheckpointing      = True
    , cfgUploadDirs         = []
    }

-- | Strip leading and trailing whitespace.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Strip matching surrounding quotes (single or double) from a value.
stripQuotes :: String -> String
stripQuotes s
    | length s >= 2
    , let q = head s
    , q == '"' || q == '\''
    , last s == q
    = tail (init s)
    | otherwise = s

-- | Parse a .env file: skip blanks/comments, parse KEY=VALUE, strip quotes,
--   setEnv (only if the key is not already in the environment).
loadEnvFile :: String -> IO ()
loadEnvFile path = do
    exists <- doesFileExist path
    if not exists
        then return ()
        else do
            contents <- readFile path
            mapM_ parseLine (lines contents)
  where
    parseLine :: String -> IO ()
    parseLine raw =
        let line = strip raw
        in if null line || "#" `isPrefixOf` line
            then return ()
            else case break (== '=') line of
                (_, "")        -> return ()  -- no '=' found
                (key, '=':val) -> do
                    let k = strip key
                        v = stripQuotes (strip val)
                    existing <- lookupEnv k
                    case existing of
                        Nothing -> setEnv k v
                        Just _  -> return ()
                _ -> return ()

-- | Helper: lookupEnv with a default value.
envOrDefault :: String -> String -> IO String
envOrDefault key def = do
    val <- lookupEnv key
    return $ case val of
        Nothing -> def
        Just ""  -> def
        Just v   -> v

-- | Load configuration from .env file and environment variables.
--   This mirrors the bash script's configuration loading order:
--   1. Read ENV_FILE env var (default "./.env")
--   2. Parse .env file (sets env vars that aren't already set)
--   3. Read all config env vars with defaults
loadConfig :: IO Config
loadConfig = do
    envFile <- envOrDefault "ENV_FILE" "./.env"

    -- Parse .env (populates env for keys not already set)
    loadEnvFile envFile

    -- SPRITE_TOKEN with SPRITES_TOKEN fallback
    spriteToken0 <- envOrDefault "SPRITE_TOKEN" ""
    spriteToken <- if null spriteToken0
        then envOrDefault "SPRITES_TOKEN" ""
        else return spriteToken0

    agent        <- envOrDefault "AGENT" "opencode"
    claudeAuth   <- envOrDefault "CLAUDE_AUTH" "subscription"
    apiKey       <- envOrDefault "ANTHROPIC_API_KEY" ""
    model        <- envOrDefault "MODEL" ""
    intervalStr  <- envOrDefault "CHECKPOINT_INTERVAL" "300"

    interval <- case reads intervalStr :: [(Int, String)] of
        [(n, "")] -> return n
        _         -> do
            hPutStrLn stderr $
                "Error: invalid CHECKPOINT_INTERVAL " ++ show intervalStr
                ++ " (must be integer)"
            exitFailure

    return defaultConfig
        { cfgSpriteToken        = spriteToken
        , cfgAgent              = agent
        , cfgClaudeAuth         = claudeAuth
        , cfgAnthropicApiKey    = apiKey
        , cfgModel              = model
        , cfgCheckpointInterval = interval
        , cfgEnvFile            = envFile
        }
