# sprite-tool — Ruby port

Port of [sprite-scripts](../../README.md) as a single CLI with subcommands: `launch`, `push`, `pull`, `watch`.

## Prerequisites

- Ruby >= 3.0

## Build

N/A (interpreted). Optional:

```sh
bundle install
```

## Run

```sh
ruby bin/sprite-tool launch my-sprite plan.md
```

## Project structure

```
Gemfile
bin/
    sprite-tool
lib/
    sprite_tool.rb
    sprite_tool/
        cli.rb
        config.rb
        sprite.rb
        launch.rb
        push.rb
        pull.rb
        watch.rb
```
