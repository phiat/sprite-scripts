"""sprite-tool push: Push local file or directory to a sprite."""

from python import Python, PythonObject


def _usage() raises:
    print(
        "Usage: sprite-tool push <local-path> <remote-path> [sprite-name]\n"
        "\n"
        "Examples:\n"
        "  sprite-tool push ./file.txt /home/sprite/file.txt\n"
        "  sprite-tool push ./mydir /home/sprite/mydir\n"
        "  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk"
    )
    var sys_mod = Python.import_module("sys")
    _ = sys_mod.exit(1)


def run(args: PythonObject) raises:
    """Execute the push subcommand."""
    var sys_mod = Python.import_module("sys")
    var os = Python.import_module("os")
    var subprocess = Python.import_module("subprocess")
    var builtins = Python.import_module("builtins")

    if int(len(args)) < 2:
        _usage()

    var local_path = str(args[0])
    var remote_path = str(args[1])
    var sprite_name: String = ""
    if int(len(args)) > 2:
        sprite_name = str(args[2])

    var sprite_args = Python.list()
    if len(sprite_name) > 0:
        sprite_args.append("-s")
        sprite_args.append(sprite_name)

    if not bool(os.path.exists(local_path)):
        # Print to stderr
        print("Error: " + local_path + " does not exist")
        _ = sys_mod.exit(1)

    if bool(os.path.isdir(local_path)):
        print("Pushing directory: " + local_path + " -> " + remote_path)
        var parent = str(os.path.dirname(local_path))
        var base = str(os.path.basename(local_path))

        # tar czf - -C parent base
        var tar_args = Python.list()
        tar_args.append("tar")
        tar_args.append("czf")
        tar_args.append("-")
        tar_args.append("-C")
        tar_args.append(parent)
        tar_args.append(base)

        # sprite exec ... bash -c "mkdir -p DEST && tar xzf - -C DEST --strip-components=1"
        var sprite_cmd = Python.list()
        sprite_cmd.append("sprite")
        sprite_cmd.append("exec")
        for i in range(int(len(sprite_args))):
            sprite_cmd.append(sprite_args[i])
        sprite_cmd.append("bash")
        sprite_cmd.append("-c")
        sprite_cmd.append(
            "mkdir -p '" + remote_path + "' && tar xzf - -C '" + remote_path + "' --strip-components=1"
        )

        var tar_proc = subprocess.Popen(tar_args, stdout=subprocess.PIPE)
        var sprite_proc = subprocess.Popen(sprite_cmd, stdin=tar_proc.stdout)
        tar_proc.stdout.close()
        _ = sprite_proc.communicate()
        _ = tar_proc.wait()

        if int(sprite_proc.returncode) != 0:
            print("Error: push failed")
            _ = sys_mod.exit(1)
    else:
        print("Pushing file: " + local_path + " -> " + remote_path)
        var remote_dir = str(os.path.dirname(remote_path))

        var sprite_cmd = Python.list()
        sprite_cmd.append("sprite")
        sprite_cmd.append("exec")
        for i in range(int(len(sprite_args))):
            sprite_cmd.append(sprite_args[i])
        sprite_cmd.append("bash")
        sprite_cmd.append("-c")
        sprite_cmd.append("mkdir -p '" + remote_dir + "' && cat > '" + remote_path + "'")

        var fh = builtins.open(local_path, "rb")
        var result = subprocess.run(sprite_cmd, stdin=fh)
        fh.close()
        if int(result.returncode) != 0:
            print("Error: push failed")
            _ = sys_mod.exit(1)

    print("Done.")
