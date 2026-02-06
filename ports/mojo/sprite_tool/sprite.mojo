"""Subprocess wrapper for the sprite CLI."""

from python import Python, PythonObject


def find_sprite_cli() raises -> String:
    """Find the sprite CLI binary on PATH. Returns empty string if not found."""
    var shutil = Python.import_module("shutil")
    var result = shutil.which("sprite")
    if result is Python.none():
        return ""
    return str(result)


def run_cmd(
    args: PythonObject,
    check: Bool = True,
    capture_output: Bool = False,
    stdin_data: PythonObject = Python.none(),
    stdin_file: String = "",
    stdout_file: String = "",
) raises -> PythonObject:
    """Run a sprite CLI command.

    Args:
        args: Python list of command arguments (without leading 'sprite').
        check: Raise on non-zero exit.
        capture_output: Capture stdout/stderr.
        stdin_data: Bytes to feed to stdin (Python bytes or None).
        stdin_file: Path to file to open as stdin.
        stdout_file: Path to file to open as stdout.
    """
    var subprocess = Python.import_module("subprocess")
    var builtins = Python.import_module("builtins")

    var cmd = Python.list()
    cmd.append("sprite")
    for i in range(len(args)):
        cmd.append(args[i])

    var opened_files = Python.list()
    var stdin_handle = Python.none()
    var stdout_handle = Python.none()

    if stdin_data is not Python.none():
        stdin_handle = subprocess.PIPE
    elif len(stdin_file) > 0:
        var fh = builtins.open(stdin_file, "rb")
        opened_files.append(fh)
        stdin_handle = fh

    if len(stdout_file) > 0:
        var fh = builtins.open(stdout_file, "wb")
        opened_files.append(fh)
        stdout_handle = fh
    elif capture_output:
        stdout_handle = subprocess.PIPE

    var stderr_handle = Python.none()
    if capture_output:
        stderr_handle = subprocess.PIPE

    var input_data = Python.none()
    if stdin_data is not Python.none():
        input_data = stdin_data

    var result = subprocess.run(
        cmd,
        stdin=stdin_handle,
        stdout=stdout_handle,
        stderr=stderr_handle,
        input=input_data,
        check=check,
    )

    # Close any opened file handles
    for i in range(len(opened_files)):
        opened_files[i].close()

    return result


def sx(sprite_name: String, cmd: String, dry_run: Bool = False) raises -> String:
    """Run a command inside a sprite via bash.

    Equivalent to: sprite exec -s SPRITE bash -c "CMD"

    Returns stdout as string.
    """
    if dry_run:
        print('  [dry-run] sprite exec -s ' + sprite_name + ' bash -c "' + cmd + '"')
        return ""

    var subprocess = Python.import_module("subprocess")

    var proc_args = Python.list()
    proc_args.append("sprite")
    proc_args.append("exec")
    proc_args.append("-s")
    proc_args.append(sprite_name)
    proc_args.append("bash")
    proc_args.append("-c")
    proc_args.append(cmd)

    var result = subprocess.run(
        proc_args,
        capture_output=True,
        text=True,
    )
    var stdout = result.stdout
    if stdout is Python.none():
        return ""
    return str(stdout).strip()


def sx_passthrough(sprite_name: String, cmd: String, dry_run: Bool = False) raises -> Int:
    """Run a command inside a sprite, passing stdout/stderr through to terminal.

    Returns the exit code.
    """
    if dry_run:
        print('  [dry-run] sprite exec -s ' + sprite_name + ' bash -c "' + cmd + '"')
        return 0

    var subprocess = Python.import_module("subprocess")

    var proc_args = Python.list()
    proc_args.append("sprite")
    proc_args.append("exec")
    proc_args.append("-s")
    proc_args.append(sprite_name)
    proc_args.append("bash")
    proc_args.append("-c")
    proc_args.append(cmd)

    var result = subprocess.run(proc_args)
    return int(result.returncode)


def push_file(sprite_name: String, src: String, dest: String, dry_run: Bool = False) raises:
    """Push a local file to a sprite.

    Equivalent to:
        sprite exec -s SPRITE bash -c "mkdir -p $(dirname DEST)"
        sprite exec -s SPRITE bash -c "cat > DEST" < src
    """
    if dry_run:
        print("  [dry-run] push " + src + " -> sprite:" + dest)
        return

    var subprocess = Python.import_module("subprocess")
    var os = Python.import_module("os")
    var builtins = Python.import_module("builtins")

    var dest_dir = str(os.path.dirname(dest))

    # mkdir -p dest_dir
    var mkdir_args = Python.list()
    mkdir_args.append("sprite")
    mkdir_args.append("exec")
    mkdir_args.append("-s")
    mkdir_args.append(sprite_name)
    mkdir_args.append("bash")
    mkdir_args.append("-c")
    mkdir_args.append("mkdir -p '" + dest_dir + "'")
    _ = subprocess.run(mkdir_args, check=True)

    # cat > dest < src
    var cat_args = Python.list()
    cat_args.append("sprite")
    cat_args.append("exec")
    cat_args.append("-s")
    cat_args.append(sprite_name)
    cat_args.append("bash")
    cat_args.append("-c")
    cat_args.append("cat > '" + dest + "'")

    var fh = builtins.open(src, "rb")
    _ = subprocess.run(cat_args, stdin=fh, check=True)
    fh.close()


def push_dir(sprite_name: String, src: String, dest: String, dry_run: Bool = False) raises:
    """Push a local directory to a sprite via tar.

    Equivalent to:
        sprite exec -s SPRITE bash -c "mkdir -p DEST"
        tar czf - -C parent base | sprite exec -s SPRITE bash -c "tar xzf - -C parent_of_dest"
    """
    if dry_run:
        print("  [dry-run] push dir " + src + " -> sprite:" + dest)
        return

    var subprocess = Python.import_module("subprocess")
    var os = Python.import_module("os")

    var parent = str(os.path.dirname(src))
    var base = str(os.path.basename(src))
    var dest_parent = str(os.path.dirname(dest))

    # mkdir -p dest
    var mkdir_args = Python.list()
    mkdir_args.append("sprite")
    mkdir_args.append("exec")
    mkdir_args.append("-s")
    mkdir_args.append(sprite_name)
    mkdir_args.append("bash")
    mkdir_args.append("-c")
    mkdir_args.append("mkdir -p '" + dest + "'")
    _ = subprocess.run(mkdir_args, check=True)

    # tar czf - -C parent base | sprite exec ... bash -c "tar xzf - -C dest_parent"
    var tar_args = Python.list()
    tar_args.append("tar")
    tar_args.append("czf")
    tar_args.append("-")
    tar_args.append("-C")
    tar_args.append(parent)
    tar_args.append(base)

    var sprite_args = Python.list()
    sprite_args.append("sprite")
    sprite_args.append("exec")
    sprite_args.append("-s")
    sprite_args.append(sprite_name)
    sprite_args.append("bash")
    sprite_args.append("-c")
    sprite_args.append("tar xzf - -C '" + dest_parent + "'")

    var tar_proc = subprocess.Popen(tar_args, stdout=subprocess.PIPE)
    var sprite_proc = subprocess.Popen(sprite_args, stdin=tar_proc.stdout)
    tar_proc.stdout.close()  # Allow SIGPIPE propagation
    _ = sprite_proc.communicate()
    _ = tar_proc.wait()

    if int(sprite_proc.returncode) != 0:
        print("Error: push_dir failed")
        var sys_mod = Python.import_module("sys")
        _ = sys_mod.exit(1)


def sprite_list() raises -> String:
    """Run 'sprite ls' and return stdout."""
    var subprocess = Python.import_module("subprocess")

    var args = Python.list()
    args.append("sprite")
    args.append("ls")

    var result = subprocess.run(args, capture_output=True, text=True)
    var stdout = result.stdout
    if stdout is Python.none():
        return ""
    return str(stdout)


def sprite_exists(sprite_name: String) raises -> Bool:
    """Check if a sprite with the given name already exists."""
    var output = sprite_list()
    var lines = output.split("\n")
    for i in range(len(lines)):
        var line = lines[i]
        if sprite_name in str(line):
            # Check as whole word by examining split parts
            var parts = str(line).split()
            for j in range(len(parts)):
                if str(parts[j]) == sprite_name:
                    return True
    return False
