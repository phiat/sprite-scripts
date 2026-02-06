# sprite-tool — Python port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Python >= 3.10

## Build

N/A (interpreted). Optional:

```sh
pip install .
```

## Run

```sh
python -m sprite_tool launch my-sprite plan.md
```

## Project structure

```
pyproject.toml
sprite_tool/
    __init__.py
    __main__.py
    cli.py
    config.py
    sprite.py
    launch.py
    push.py
    pull.py
    watch.py
```
