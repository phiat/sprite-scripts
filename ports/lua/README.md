# sprite-tool — Lua port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Lua >= 5.3 or LuaJIT

## Build

N/A (interpreted).

## Run

```sh
lua bin/sprite-tool launch my-sprite plan.md
```

## Project structure

```
bin/
    sprite-tool
lib/
    config.lua
    sprite.lua
    launch.lua
    push.lua
    pull.lua
    watch.lua
```
