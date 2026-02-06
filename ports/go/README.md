# sprite-tool — Go port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Go >= 1.21

## Build

```sh
go build -o sprite-tool ./cmd/sprite-tool
```

## Run

```sh
./sprite-tool launch my-sprite plan.md
```

## Project structure

```
go.mod
cmd/sprite-tool/
  main.go
  launch.go
  push.go
  pull.go
  watch.go
internal/
  sprite/
  agent/
  config/
```
