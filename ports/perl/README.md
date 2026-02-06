# sprite-tool — Perl port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Perl >= 5.26

## Build

N/A (interpreted).

## Run

```sh
perl -Ilib bin/sprite-tool launch my-sprite plan.md
```

## Project structure

```
bin/
    sprite-tool
lib/
    SpriteExec.pm
    Config.pm
    Launch.pm
    Push.pm
    Pull.pm
    Watch.pm
```
