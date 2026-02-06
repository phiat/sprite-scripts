package main

import "core:fmt"
import "core:os"

cmd_push :: proc(args: []string) {
    if len(args) < 2 {
        push_usage()
        os.exit(1)
    }

    // Check for help flag
    for arg in args {
        if arg == "--help" || arg == "-h" {
            push_usage()
            os.exit(0)
        }
    }

    local_path := args[0]
    remote_path := args[1]
    sprite_name := ""
    if len(args) >= 3 {
        sprite_name = args[2]
    }

    // Check if local path exists
    if !file_exists(local_path) && !is_directory(local_path) {
        fmt.eprintf("Error: %s does not exist\n", local_path)
        os.exit(1)
    }

    // Build sprite args portion
    sprite_flag := ""
    if sprite_name != "" {
        sprite_flag = fmt.tprintf("-s %s ", sprite_name)
    }

    if is_directory(local_path) {
        fmt.printf("Pushing directory: %s -> %s\n", local_path, remote_path)

        parent := dir_name(local_path)
        base := base_name(local_path)

        cmd := fmt.tprintf(
            "tar czf - -C '%s' '%s' | sprite exec %sbash -c \"mkdir -p '%s' && tar xzf - -C '%s' --strip-components=1\"",
            parent, base, sprite_flag, remote_path, remote_path,
        )
        run_cmd(cmd)
    } else {
        fmt.printf("Pushing file: %s -> %s\n", local_path, remote_path)

        remote_dir := dir_name(remote_path)

        cmd := fmt.tprintf(
            "sprite exec %sbash -c \"mkdir -p '%s' && cat > '%s'\" < %s",
            sprite_flag, remote_dir, remote_path, local_path,
        )
        run_cmd(cmd)
    }

    fmt.println("Done.")
}

push_usage :: proc() {
    fmt.eprintln("Usage: sprite-tool push <local-path> <remote-path> [sprite-name]")
    fmt.eprintln("")
    fmt.eprintln("Examples:")
    fmt.eprintln("  sprite-tool push ./file.txt /home/sprite/file.txt")
    fmt.eprintln("  sprite-tool push ./mydir /home/sprite/mydir")
    fmt.eprintln("  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk")
}
