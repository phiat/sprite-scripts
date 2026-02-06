"""sprite-tool pull: Pull file or directory from a sprite."""

from python import Python, PythonObject


def _usage() raises:
    print(
        "Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]\n"
        "\n"
        "Examples:\n"
        "  sprite-tool pull /home/sprite/file.txt ./file.txt\n"
        "  sprite-tool pull /home/sprite/mydir ./mydir\n"
        "  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk"
    )
    var sys_mod = Python.import_module("sys")
    _ = sys_mod.exit(1)


def run(args: PythonObject) raises:
    """Execute the pull subcommand."""
    var sys_mod = Python.import_module("sys")
    var os = Python.import_module("os")
    var subprocess = Python.import_module("subprocess")
    var builtins = Python.import_module("builtins")

    if int(len(args)) < 2:
        _usage()

    var remote_path = str(args[0])
    var local_path = str(args[1])
    var sprite_name: String = ""
    if int(len(args)) > 2:
        sprite_name = str(args[2])

    var sprite_args = Python.list()
    if len(sprite_name) > 0:
        sprite_args.append("-s")
        sprite_args.append(sprite_name)

    # Check if remote is directory or file
    var check_cmd = Python.list()
    check_cmd.append("sprite")
    check_cmd.append("exec")
    for i in range(int(len(sprite_args))):
        check_cmd.append(sprite_args[i])
    check_cmd.append("bash")
    check_cmd.append("-c")
    check_cmd.append("[ -d '" + remote_path + "' ] && echo dir || echo file")

    var check_result = subprocess.run(check_cmd, capture_output=True, text=True)
    var is_dir = str(check_result.stdout).strip() == "dir"

    if is_dir:
        print("Pulling directory: " + remote_path + " -> " + local_path)
        _ = os.makedirs(local_path, exist_ok=True)

        # sprite exec ... tar czf - -C REMOTE . | tar xzf - -C LOCAL
        var sprite_cmd = Python.list()
        sprite_cmd.append("sprite")
        sprite_cmd.append("exec")
        for i in range(int(len(sprite_args))):
            sprite_cmd.append(sprite_args[i])
        sprite_cmd.append("tar")
        sprite_cmd.append("czf")
        sprite_cmd.append("-")
        sprite_cmd.append("-C")
        sprite_cmd.append(remote_path)
        sprite_cmd.append(".")

        var tar_cmd = Python.list()
        tar_cmd.append("tar")
        tar_cmd.append("xzf")
        tar_cmd.append("-")
        tar_cmd.append("-C")
        tar_cmd.append(local_path)

        var sprite_proc = subprocess.Popen(sprite_cmd, stdout=subprocess.PIPE)
        var tar_proc = subprocess.Popen(tar_cmd, stdin=sprite_proc.stdout)
        sprite_proc.stdout.close()
        _ = tar_proc.communicate()
        _ = sprite_proc.wait()

        if int(tar_proc.returncode) != 0:
            print("Error: pull failed")
            _ = sys_mod.exit(1)
    else:
        print("Pulling file: " + remote_path + " -> " + local_path)
        # Ensure parent directory exists
        var parent_dir = str(os.path.dirname(local_path))
        if len(parent_dir) > 0:
            _ = os.makedirs(parent_dir, exist_ok=True)

        # sprite exec ... cat REMOTE > local
        var sprite_cmd = Python.list()
        sprite_cmd.append("sprite")
        sprite_cmd.append("exec")
        for i in range(int(len(sprite_args))):
            sprite_cmd.append(sprite_args[i])
        sprite_cmd.append("cat")
        sprite_cmd.append(remote_path)

        var fh = builtins.open(local_path, "wb")
        var result = subprocess.run(sprite_cmd, stdout=fh)
        fh.close()
        if int(result.returncode) != 0:
            print("Error: pull failed")
            _ = sys_mod.exit(1)

    print("Done.")
