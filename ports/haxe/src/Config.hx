import sys.io.File;
import sys.FileSystem;

/**
 * Config handles loading environment variables from a .env file
 * and providing access to configuration values.
 */
class Config {
    /**
     * Load a .env file, parsing KEY=VALUE lines and setting them
     * via Sys.putEnv(). Skips comments and blank lines, strips quotes.
     */
    public static function loadEnv(envFile:String):Void {
        if (!FileSystem.exists(envFile)) {
            return;
        }
        var content = File.getContent(envFile);
        var lines = content.split("\n");
        for (line in lines) {
            var trimmed = StringTools.trim(line);
            // Skip blank lines and comments
            if (trimmed.length == 0 || StringTools.startsWith(trimmed, "#")) {
                continue;
            }
            var eqIdx = trimmed.indexOf("=");
            if (eqIdx < 1) {
                continue;
            }
            var key = StringTools.trim(trimmed.substr(0, eqIdx));
            var value = StringTools.trim(trimmed.substr(eqIdx + 1));
            // Strip surrounding quotes (single or double)
            if (value.length >= 2) {
                var first = value.charAt(0);
                var last = value.charAt(value.length - 1);
                if ((first == '"' && last == '"') || (first == "'" && last == "'")) {
                    value = value.substr(1, value.length - 2);
                }
            }
            Sys.putEnv(key, value);
        }
    }

    /**
     * Get an environment variable with a fallback default.
     */
    public static function getEnvOr(key:String, fallback:String):String {
        var val = Sys.getEnv(key);
        if (val == null || val.length == 0) {
            return fallback;
        }
        return val;
    }

    /**
     * Get an environment variable, returning empty string if not set.
     */
    public static function getEnvOrEmpty(key:String):String {
        var val = Sys.getEnv(key);
        if (val == null) {
            return "";
        }
        return val;
    }
}
