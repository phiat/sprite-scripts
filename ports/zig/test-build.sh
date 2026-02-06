#!/bin/bash
cd /home/phiat/lab/feb/sprite-scripts/ports/zig
zig build 2>&1
echo "EXIT=$?"
