# sprite-tool — Clojure port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Clojure CLI (clj)
- Java >= 11

## Build

N/A (interpreted)

## Run

```sh
clj -M -m sprite-tool.core launch my-sprite plan.md
```

## Project structure

```
deps.edn
src/sprite_tool/
  core.clj
  config.clj
  sprite.clj
  launch.clj
  push.clj
  pull.clj
  watch.clj
```
