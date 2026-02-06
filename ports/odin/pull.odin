package main

import "core:fmt"
import "core:os"
import "core:strings"

cmd_pull :: proc(args: []string) {
    if len(args) < 2 {
        pull_usage()
        os.exit(1)
    }

    // Check for help flag
    for arg in args {
        if arg == "--help" || arg == "-h" {
            pull_usage()
            os.exit(0)
        }
    }

    remote_path := args[0]
    local_path := args[1]
    sprite_name := ""
    if len(args) >= 3 {
        sprite_name = args[2]
    }

    // Build sprite args portion
    sprite_flag := ""
    if sprite_name != "" {
        sprite_flag = fmt.tprintf("-s %s ", sprite_name)
    }

    // Check if remote path is a directory
    check_cmd := fmt.tprintf(
        "sprite exec %sbash -c \"[ -d '%s' ] && echo 'dir' || echo 'file'\" 2>/dev/null",
        sprite_flag, remote_path,
    )
    is_dir_output := run_capture(check_cmd)
    is_dir := strings.trim_space(is_dir_output) == "dir"

    if is_dir {
        fmt.printf("Pulling directory: %s -> %s\n", remote_path, local_path)

        // mkdir -p local_path
        mkdir_cmd := fmt.tprintf("mkdir -p '%s'", local_path)
        run_cmd(mkdir_cmd)

        // Pipe sprite tar to local tar
        cmd := fmt.tprintf(
            "sprite exec %sbash -c \"tar czf - -C '%s' .\" | tar xzf - -C '%s'",
            sprite_flag, remote_path, local_path,
        )
        ret := run_cmd(cmd)
        if ret != 0 {
            fmt.eprintln("Error: pull directory failed")
            os.exit(1)
        }
    } else {
        fmt.printf("Pulling file: %s -> %s\n", remote_path, local_path)

        // mkdir -p dirname(local_path)
        local_dir := dir_name(local_path)
        mkdir_cmd := fmt.tprintf("mkdir -p '%s'", local_dir)
        run_cmd(mkdir_cmd)

        // sprite exec cat remote_path > local_path
        cmd := fmt.tprintf(
            "sprite exec %scat '%s' > '%s'",
            sprite_flag, remote_path, local_path,
        )
        ret := run_cmd(cmd)
        if ret != 0 {
            fmt.eprintln("Error: pull file failed")
            os.exit(1)
        }
    }

    fmt.println("Done.")
}

pull_usage :: proc() {
    fmt.eprintln("Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]")
    fmt.eprintln("")
    fmt.eprintln("Examples:")
    fmt.eprintln("  sprite-tool pull /home/sprite/file.txt ./file.txt")
    fmt.eprintln("  sprite-tool pull /home/sprite/mydir ./mydir")
    fmt.eprintln("  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk")
}
