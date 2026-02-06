# sprite-tool — D port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- D compiler (DMD or LDC)
- DUB package manager

## Build

```sh
dub build
```

## Run

```sh
./sprite-tool launch my-sprite plan.md
```

## Project structure

```
dub.json
source/
  app.d
  config.d
  sprite.d
  launch.d
  push.d
  pull.d
  watch.d
```
