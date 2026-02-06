#ifndef SPRITE_H
#define SPRITE_H

#include <stdbool.h>
#include <sys/types.h>

/*
 * Run a command inside a sprite via: sprite exec -s SPRITE bash -c "CMD"
 * If dry_run is true, prints the command instead of executing.
 * Returns system() exit status (0 on success).
 */
int sx(const char *sprite, const char *cmd, bool dry_run);

/*
 * Run a command inside a sprite and capture stdout into buf (up to bufsz-1 bytes).
 * Returns 0 on success, -1 on failure. buf is NUL-terminated.
 */
int sx_capture(const char *sprite, const char *cmd, char *buf, size_t bufsz);

/*
 * Push a local file to a sprite at the given remote path.
 * If dry_run, prints what would happen.
 * If sprite is NULL, uses the default sprite.
 * Returns 0 on success.
 */
int push_file(const char *sprite, const char *local_path,
              const char *remote_path, bool dry_run);

/*
 * Push a local directory to a sprite at the given remote path.
 * Uses tar czf piped to sprite exec tar xzf.
 * If sprite is NULL, uses the default sprite.
 * Returns 0 on success.
 */
int push_dir(const char *sprite, const char *local_dir,
             const char *remote_path, bool dry_run);

/*
 * Pull a file from a sprite to a local path.
 * Returns 0 on success.
 */
int pull_file(const char *sprite, const char *remote_path,
              const char *local_path);

/*
 * Pull a directory from a sprite to a local path.
 * Uses sprite exec tar czf piped to local tar xzf.
 * Returns 0 on success.
 */
int pull_dir(const char *sprite, const char *remote_path,
             const char *local_path);

/*
 * Check whether a remote path is a directory.
 * Returns 1 if directory, 0 if file, -1 on error.
 */
int remote_is_dir(const char *sprite, const char *remote_path);

#endif /* SPRITE_H */
