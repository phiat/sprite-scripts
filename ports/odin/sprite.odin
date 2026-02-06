package main

import "core:fmt"
import "core:strings"
import "core:c"
import "core:c/libc"
import "core:os"

foreign import posix_proc "system:c"

foreign posix_proc {
    @(link_name = "popen")
    _popen :: proc "c" (command: cstring, mode: cstring) -> ^libc.FILE ---

    @(link_name = "pclose")
    _pclose :: proc "c" (stream: ^libc.FILE) -> c.int ---
}

// Run a shell command via libc.system(). Returns the exit code.
run_cmd :: proc(cmd: string) -> i32 {
    c_cmd := strings.clone_to_cstring(cmd)
    return libc.system(c_cmd)
}

// Run a shell command and capture its stdout via popen().
// Returns the captured output as a string, or "" on failure.
run_capture :: proc(cmd: string) -> string {
    c_cmd := strings.clone_to_cstring(cmd)
    fp := _popen(c_cmd, "r")
    if fp == nil {
        return ""
    }

    buf: [4096]u8
    result := strings.builder_make()

    for {
        ret := libc.fgets(raw_data(&buf), 4096, fp)
        if ret == nil {
            break
        }
        // Convert the C string in buf to an Odin string
        chunk := string(cstring(raw_data(&buf)))
        strings.write_string(&result, chunk)
    }

    _pclose(fp)
    return strings.trim_space(strings.to_string(result))
}

// Execute a bash command on a sprite: sprite exec -s <name> bash -c <cmd>
// If dry_run is true, prints the command instead of executing.
// Returns the exit code (0 for dry-run).
sx :: proc(sprite_name: string, cmd: string, dry_run: bool) -> i32 {
    if dry_run {
        fmt.printf("  [dry-run] sprite exec -s %s bash -c \"%s\"\n", sprite_name, cmd)
        return 0
    }

    full_cmd := fmt.tprintf("sprite exec -s %s bash -c '%s'", sprite_name, cmd)
    return run_cmd(full_cmd)
}

// Execute a bash command on a sprite and capture its stdout.
sx_capture :: proc(sprite_name: string, cmd: string) -> string {
    full_cmd := fmt.tprintf("sprite exec -s %s bash -c '%s' 2>/dev/null", sprite_name, cmd)
    return run_capture(full_cmd)
}

// Push a local file to a sprite.
push_file :: proc(sprite_name: string, local_path: string, remote_path: string, dry_run: bool) {
    if dry_run {
        fmt.printf("  [dry-run] push %s -> sprite:%s\n", local_path, remote_path)
        return
    }

    // Get the directory portion of remote_path
    remote_dir := dir_name(remote_path)

    // mkdir -p on remote
    mkdir_cmd := fmt.tprintf("mkdir -p '%s'", remote_dir)
    sx(sprite_name, mkdir_cmd, false)

    // cat > remote_path < local_path
    cat_cmd := fmt.tprintf("sprite exec -s %s bash -c \"cat > '%s'\" < %s",
                           sprite_name, remote_path, local_path)
    run_cmd(cat_cmd)
}

// Push a local directory to a sprite via tar pipe.
push_dir :: proc(sprite_name: string, local_path: string, remote_path: string, dry_run: bool) {
    if dry_run {
        fmt.printf("  [dry-run] push dir %s -> sprite:%s\n", local_path, remote_path)
        return
    }

    // mkdir -p on remote
    mkdir_cmd := fmt.tprintf("mkdir -p '%s'", remote_path)
    sx(sprite_name, mkdir_cmd, false)

    parent := dir_name(local_path)
    base := base_name(local_path)
    remote_parent := dir_name(remote_path)

    tar_cmd := fmt.tprintf(
        "tar czf - -C '%s' '%s' | sprite exec -s %s bash -c \"tar xzf - -C '%s'\"",
        parent, base, sprite_name, remote_parent,
    )
    run_cmd(tar_cmd)
}

// Check if the sprite CLI is installed.
is_sprite_installed :: proc() -> bool {
    ret := run_cmd("command -v sprite >/dev/null 2>&1")
    return ret == 0
}

// Check if a local file exists.
file_exists :: proc(path: string) -> bool {
    return os.exists(path)
}

// Check if a local path is a directory.
is_directory :: proc(path: string) -> bool {
    if !os.exists(path) {
        return false
    }
    // Use shell test to check directory, avoids platform-specific stat struct issues
    check := fmt.tprintf("test -d '%s'", path)
    return run_cmd(check) == 0
}

// Extract directory name from a path (everything before the last /).
dir_name :: proc(path: string) -> string {
    idx := strings.last_index(path, "/")
    if idx < 0 {
        return "."
    }
    if idx == 0 {
        return "/"
    }
    return path[:idx]
}

// Extract base name from a path (everything after the last /).
base_name :: proc(path: string) -> string {
    idx := strings.last_index(path, "/")
    if idx < 0 {
        return path
    }
    return path[idx + 1:]
}
