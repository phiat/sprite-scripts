"""Configuration: .env parsing and environment variable loading."""

import os
from dataclasses import dataclass, field
from pathlib import Path
from typing import List


@dataclass
class Config:
    """All configuration derived from environment variables and .env files."""

    sprite_token: str = ""
    agent: str = "opencode"
    claude_auth: str = "subscription"
    anthropic_api_key: str = ""
    model: str = ""
    checkpoint_interval: int = 300
    env_file: str = "./.env"

    # CLI flags
    dry_run: bool = False
    checkpointing: bool = True
    upload_dirs: List[str] = field(default_factory=list)


def parse_env_file(path: str) -> dict:
    """Hand-rolled .env parser.

    Skips blank lines and comments. Parses KEY=VALUE with optional
    single/double quote stripping. Sets parsed values into os.environ
    (does not overwrite existing env vars).
    """
    env_path = Path(path)
    if not env_path.is_file():
        return {}

    parsed = {}
    with open(env_path, "r") as f:
        for line in f:
            line = line.strip()
            # Skip blank lines and comments
            if not line or line.startswith("#"):
                continue
            # Must contain =
            if "=" not in line:
                continue
            key, _, value = line.partition("=")
            key = key.strip()
            value = value.strip()
            # Strip matching quotes
            if len(value) >= 2 and value[0] == value[-1] and value[0] in ('"', "'"):
                value = value[1:-1]
            parsed[key] = value
            # Set in os.environ (don't overwrite existing)
            if key not in os.environ:
                os.environ[key] = value

    return parsed


def load() -> Config:
    """Load configuration from .env file and environment variables."""
    env_file = os.environ.get("ENV_FILE", "./.env")

    # Parse .env file first (populates os.environ for keys not already set)
    parse_env_file(env_file)

    # SPRITE_TOKEN with SPRITES_TOKEN fallback
    sprite_token = os.environ.get("SPRITE_TOKEN", "")
    if not sprite_token:
        sprite_token = os.environ.get("SPRITES_TOKEN", "")

    # Agent
    agent = os.environ.get("AGENT", "opencode")

    # Claude auth
    claude_auth = os.environ.get("CLAUDE_AUTH", "subscription")

    # Anthropic API key
    anthropic_api_key = os.environ.get("ANTHROPIC_API_KEY", "")

    # Model
    model = os.environ.get("MODEL", "")

    # Checkpoint interval
    interval_str = os.environ.get("CHECKPOINT_INTERVAL", "300")
    try:
        checkpoint_interval = int(interval_str)
    except ValueError:
        raise SystemExit(
            f"Error: invalid CHECKPOINT_INTERVAL {interval_str!r} (must be integer)"
        )

    return Config(
        sprite_token=sprite_token,
        agent=agent,
        claude_auth=claude_auth,
        anthropic_api_key=anthropic_api_key,
        model=model,
        checkpoint_interval=checkpoint_interval,
        env_file=env_file,
    )
