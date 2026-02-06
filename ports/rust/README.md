# sprite-tool — Rust port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Rust toolchain (cargo)

## Build

```sh
cargo build --release
```

## Run

```sh
./target/release/sprite-tool launch my-sprite plan.md
```

## Project structure

```
Cargo.toml
src/
  main.rs
  lib.rs
  launch.rs
  push.rs
  pull.rs
  watch.rs
```
