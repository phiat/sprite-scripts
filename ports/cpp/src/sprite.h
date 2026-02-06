#pragma once

#include <string>
#include <vector>
#include <optional>

// Run a command inside a sprite via: sprite exec -s <sprite> bash -c '<cmd>'
// If dry_run is true, prints what would run and returns 0.
int sx(const std::string& sprite, const std::string& cmd, bool dry_run);

// Run a command and capture its stdout. Returns the trimmed output.
std::string capture(const std::string& cmd);

// Push a local file to a remote path on a sprite.
// If dry_run, prints what would happen.
int push_file(const std::string& sprite, const std::string& src, const std::string& dest, bool dry_run);

// Push a local directory to a remote path on a sprite via tar piping.
// If dry_run, prints what would happen.
int push_dir(const std::string& sprite, const std::string& src, const std::string& dest, bool dry_run);

// Build sprite exec argument string with optional -s flag.
std::string sprite_args(const std::optional<std::string>& sprite_name);

// Shell-escape a string for safe embedding in single-quoted shell contexts.
std::string shell_escape(const std::string& s);
