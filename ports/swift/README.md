# sprite-tool — Swift port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Swift >= 5.9

## Build

```sh
swift build -c release
```

## Run

```sh
.build/release/sprite-tool launch my-sprite plan.md
```

## Project structure

```
Package.swift
Sources/
  main.swift
  Config.swift
  SpriteExec.swift
  Launch.swift
  Push.swift
  Pull.swift
  Watch.swift
```
