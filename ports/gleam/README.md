# sprite-tool — Gleam port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Gleam
- Erlang/OTP

## Build

```sh
gleam build
```

## Run

```sh
gleam run -- launch my-sprite plan.md
```

## Project structure

```
gleam.toml
manifest.toml
src/
  sprite_tool.gleam
  config.gleam
  sprite.gleam
  launch.gleam
  push.gleam
  pull.gleam
  watch.gleam
```
