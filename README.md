# sprite-scripts

CLI tools for launching and managing coding agents on [sprites.dev](https://sprites.dev) cloud environments.

Spin up a sprite, push your plan, launch Claude or OpenCode, and let it build while you grab coffee. Auto-checkpointing saves your work. Poll progress from your host machine via beads.

## Scripts

| Script | Description |
|--------|-------------|
| `sprite-launch` | Create a sprite, push files, install agent + beads, and start coding |
| `sprite-push` | Push a local file or directory to a sprite |
| `sprite-pull` | Pull a file or directory from a sprite |
| `sprite-watch` | Monitor a beads tracker task for project progress |

## Quick Start

```bash
# 1. Create a .env with your sprites.dev token
cp .env.example .env
# Edit .env with your SPRITES_TOKEN

# 2. Write a plan
cp example-plan.md plan.md
# Edit plan.md with your project spec

# 3. Launch
./sprite-launch my-project plan.md
```

## Usage

### sprite-launch

```bash
# Claude with subscription auth (default)
./sprite-launch my-project plan.md

# OpenCode with free model (no API key needed)
AGENT=opencode ./sprite-launch my-project plan.md

# Choose a model
AGENT=claude MODEL=sonnet ./sprite-launch dev plan.md
AGENT=opencode MODEL=groq/llama-3.3-70b-versatile ./sprite-launch dev plan.md

# Preview without executing
./sprite-launch --dry-run my-project plan.md

# Disable auto-checkpointing
./sprite-launch --no-checkpoint my-project plan.md

# Custom checkpoint interval (10 min)
CHECKPOINT_INTERVAL=600 ./sprite-launch my-project plan.md

# Just open a console (no plan)
./sprite-launch my-project
```

### sprite-push / sprite-pull

```bash
# Push files to sprite
./sprite-push ./src /home/sprite/src my-project
./sprite-push ./config.json /home/sprite/config.json my-project

# Pull results back
./sprite-pull /home/sprite/output ./output my-project
./sprite-pull /home/sprite/app.log ./app.log my-project
```

### sprite-watch

```bash
# Auto-detect and watch the critical tracker task
./sprite-watch my-project

# Watch a specific task
./sprite-watch my-project CRM-1

# Custom poll interval (60s)
./sprite-watch my-project CRM-1 60
```

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ENV_FILE` | `./.env` | Path to .env file |
| `SPRITES_TOKEN` | | sprites.dev API token |
| `AGENT` | `claude` | `claude` or `opencode` |
| `CLAUDE_AUTH` | `subscription` | `subscription` or `apikey` |
| `MODEL` | | Model override |
| `CHECKPOINT_INTERVAL` | `300` | Seconds between auto-checkpoints |

## Supported Models

**Claude** (requires subscription or API key):
`opus`, `sonnet`, `haiku`

**OpenCode** (free built-in models):
`opencode/big-pickle`, `opencode/glm-4.7-free`, `opencode/gpt-5-nano`, `opencode/kimi-k2.5-free`

**OpenCode + API keys** (add keys to .env):
`groq/llama-3.3-70b-versatile`, `openai/gpt-4o`, `anthropic/claude-sonnet-4-20250514`, `google/gemini-2.5-pro`

## How It Works

1. Creates (or reuses) a sprite on sprites.dev
2. Pushes your `.env` and plan file into the sprite
3. Initializes git and installs [beads](https://github.com/steveyegge/beads) for task tracking
4. Installs and authenticates your chosen coding agent
5. Starts auto-checkpointing in the background
6. Launches the agent with your plan
7. Creates a final checkpoint when the agent finishes

Use `sprite-watch` from your host to poll the agent's progress via beads.

## Requirements

- [sprites.dev](https://sprites.dev) account and CLI
- For Claude agent: Claude subscription or Anthropic API key
- For OpenCode agent: nothing (free models available), or API keys for premium models

## Links

- [sprites.dev docs](https://docs.sprites.dev/)
- [beads - git-backed issue tracker](https://github.com/steveyegge/beads)
- [OpenCode docs](https://opencode.ai/docs)
