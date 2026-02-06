package SpriteConfig;
# Config.pm - Thin wrapper that loads SpriteConfig.
# Perl's core Config.pm is reserved, so the real implementation lives in
# SpriteConfig.pm. This file exists to satisfy the spec's directory layout.
# All code should 'use SpriteConfig' and call SpriteConfig::load(), etc.

use strict;
use warnings;

require SpriteConfig;

1;
