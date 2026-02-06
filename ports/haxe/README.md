# sprite-tool — Haxe port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Haxe >= 4.3
- Neko

## Build

```sh
haxe build.hxml
```

## Run

```sh
neko sprite-tool.n launch my-sprite plan.md
```

## Project structure

```
build.hxml
src/
  Main.hx
  Config.hx
  Sprite.hx
  Launch.hx
  Push.hx
  Pull.hx
  Watch.hx
```
