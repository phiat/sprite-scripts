# sprite-tool — Zig port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Zig >= 0.15

## Build

```sh
zig build -Doptimize=ReleaseSafe
```

## Run

```sh
./zig-out/bin/sprite-tool launch my-sprite plan.md
```

## Project structure

```
build.zig
build.zig.zon
src/
  main.zig
  config.zig
  sprite.zig
  launch.zig
  push.zig
  pull.zig
  watch.zig
```
