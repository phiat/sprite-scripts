# sprite-tool — Deno port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Deno >= 2.0

## Build

N/A (interpreted). Optional:

```sh
deno compile --allow-all --output sprite-tool src/main.ts
```

## Run

```sh
deno run --allow-all src/main.ts launch my-sprite plan.md
```

## Project structure

```
deno.json
src/
    main.ts
    commands/
        launch.ts
        push.ts
        pull.ts
        watch.ts
    lib/
        config.ts
        sprite.ts
        checkpoint.ts
        transfer.ts
        agent.ts
```
