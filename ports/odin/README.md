# sprite-tool — Odin port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Odin compiler
- clang (as linker backend)

## Build

```sh
odin build . -o:speed
```

## Run

```sh
./odin launch my-sprite plan.md
```

## Project structure

```
main.odin
config.odin
sprite.odin
launch.odin
push.odin
pull.odin
watch.odin
```
