# sprite-tool — Crystal port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Crystal

## Build

```sh
crystal build src/sprite_tool.cr
```

## Run

```sh
./sprite_tool launch my-sprite plan.md
```

## Project structure

```
shard.yml
src/
  sprite_tool.cr
  config.cr
  sprite.cr
  launch.cr
  push.cr
  pull.cr
  watch.cr
```
