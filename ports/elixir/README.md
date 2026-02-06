# sprite-tool — Elixir port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Elixir >= 1.14
- Erlang/OTP

## Build

```sh
mix escript.build
```

## Run

```sh
./sprite_tool launch my-sprite plan.md
```

## Project structure

```
mix.exs
lib/
  sprite_tool.ex
  sprite_tool/
    cli.ex
    config.ex
    sprite.ex
    launch.ex
    push.ex
    pull.ex
    watch.ex
```
