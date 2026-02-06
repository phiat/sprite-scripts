#!/bin/bash
DIR="$(cd "$(dirname "$0")" && pwd)"
sbcl --noinform --non-interactive \
  --eval "(require :asdf)" \
  --eval "(push #p\"$DIR/\" asdf:*central-registry*)" \
  --eval "(asdf:load-system :sprite-tool)" \
  --eval "(sprite-tool:main)" \
  -- "$@"
