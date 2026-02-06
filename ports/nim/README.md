# sprite-tool — Nim port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Nim >= 2.0

## Build

```sh
nim c -d:release src/sprite_tool.nim
```

## Run

```sh
./src/sprite_tool launch my-sprite plan.md
```

## Project structure

```
sprite_tool.nimble
src/
  sprite_tool.nim
  config.nim
  sprite.nim
  launch.nim
  push.nim
  pull.nim
  watch.nim
```
