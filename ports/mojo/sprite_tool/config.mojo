"""Configuration: .env parsing and environment variable loading."""

from python import Python, PythonObject


struct Config:
    """All configuration derived from environment variables and .env files."""

    var sprite_token: String
    var agent: String
    var claude_auth: String
    var anthropic_api_key: String
    var model: String
    var checkpoint_interval: Int
    var env_file: String

    # CLI flags
    var dry_run: Bool
    var checkpointing: Bool
    var upload_dirs: PythonObject  # Python list of strings

    fn __init__(out self):
        self.sprite_token = ""
        self.agent = "opencode"
        self.claude_auth = "subscription"
        self.anthropic_api_key = ""
        self.model = ""
        self.checkpoint_interval = 300
        self.env_file = "./.env"
        self.dry_run = False
        self.checkpointing = True
        self.upload_dirs = PythonObject(Python.none())


def parse_env_file(path: String) raises -> PythonObject:
    """Hand-rolled .env parser.

    Skips blank lines and comments. Parses KEY=VALUE with optional
    single/double quote stripping. Sets parsed values into os.environ
    (does not overwrite existing env vars).

    Returns a Python dict of parsed key-value pairs.
    """
    var os = Python.import_module("os")
    var builtins = Python.import_module("builtins")
    var parsed = Python.dict()

    var path_obj = os.path
    if not path_obj.isfile(path):
        return parsed

    var f = builtins.open(path, "r")
    var lines = f.readlines()
    f.close()

    for i in range(len(lines)):
        var line = str(lines[i]).strip()
        # Skip blank lines and comments
        if len(line) == 0 or line[0] == "#":
            continue
        # Must contain =
        if "=" not in line:
            continue
        var eq_pos = line.find("=")
        var key = line[:eq_pos].strip()
        var value = line[eq_pos + 1 :].strip()
        # Strip matching quotes
        if len(value) >= 2:
            var first = value[0]
            var last = value[len(value) - 1]
            if first == last and (first == '"' or first == "'"):
                value = value[1 : len(value) - 1]
        parsed[key] = value
        # Set in os.environ (don't overwrite existing)
        if not os.environ.__contains__(key):
            os.environ[key] = value

    return parsed


def load() raises -> Config:
    """Load configuration from .env file and environment variables."""
    var os = Python.import_module("os")

    var env_file = str(os.environ.get("ENV_FILE", "./.env"))

    # Parse .env file first (populates os.environ for keys not already set)
    _ = parse_env_file(env_file)

    # SPRITE_TOKEN with SPRITES_TOKEN fallback
    var sprite_token = str(os.environ.get("SPRITE_TOKEN", ""))
    if len(sprite_token) == 0:
        sprite_token = str(os.environ.get("SPRITES_TOKEN", ""))

    var agent = str(os.environ.get("AGENT", "opencode"))
    var claude_auth = str(os.environ.get("CLAUDE_AUTH", "subscription"))
    var anthropic_api_key = str(os.environ.get("ANTHROPIC_API_KEY", ""))
    var model = str(os.environ.get("MODEL", ""))

    var interval_str = str(os.environ.get("CHECKPOINT_INTERVAL", "300"))
    var checkpoint_interval: Int
    try:
        checkpoint_interval = int(interval_str)
    except:
        print("Error: invalid CHECKPOINT_INTERVAL '" + interval_str + "' (must be integer)")
        var sys_mod = Python.import_module("sys")
        _ = sys_mod.exit(1)
        checkpoint_interval = 300  # unreachable, satisfies compiler

    var cfg = Config()
    cfg.sprite_token = sprite_token
    cfg.agent = agent
    cfg.claude_auth = claude_auth
    cfg.anthropic_api_key = anthropic_api_key
    cfg.model = model
    cfg.checkpoint_interval = checkpoint_interval
    cfg.env_file = env_file
    cfg.upload_dirs = Python.list()
    return cfg
