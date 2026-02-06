"""sprite-tool pull: Pull file or directory from a sprite."""

import subprocess
import sys
from pathlib import Path


def _usage():
    print("""\
Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]

Examples:
  sprite-tool pull /home/sprite/file.txt ./file.txt
  sprite-tool pull /home/sprite/mydir ./mydir
  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk""")
    sys.exit(1)


def run(args):
    """Execute the pull subcommand."""
    if len(args) < 2:
        _usage()

    remote_path = args[0]
    local_path = args[1]
    sprite_name = args[2] if len(args) > 2 else None

    sprite_args = []
    if sprite_name:
        sprite_args = ["-s", sprite_name]

    # Check if remote is directory or file
    check_cmd = (
        ["sprite", "exec"]
        + sprite_args
        + ["bash", "-c", f"[ -d '{remote_path}' ] && echo dir || echo file"]
    )
    result = subprocess.run(check_cmd, capture_output=True, text=True)
    is_dir = result.stdout.strip() == "dir"

    if is_dir:
        print(f"Pulling directory: {remote_path} -> {local_path}")
        Path(local_path).mkdir(parents=True, exist_ok=True)

        # sprite exec ... tar czf - -C REMOTE . | tar xzf - -C LOCAL
        sprite_cmd = (
            ["sprite", "exec"]
            + sprite_args
            + ["tar", "czf", "-", "-C", remote_path, "."]
        )
        sprite_proc = subprocess.Popen(
            sprite_cmd,
            stdout=subprocess.PIPE,
        )
        tar_proc = subprocess.Popen(
            ["tar", "xzf", "-", "-C", local_path],
            stdin=sprite_proc.stdout,
        )
        sprite_proc.stdout.close()
        tar_proc.communicate()
        sprite_proc.wait()

        if tar_proc.returncode != 0:
            print("Error: pull failed", file=sys.stderr)
            sys.exit(1)
    else:
        print(f"Pulling file: {remote_path} -> {local_path}")
        local = Path(local_path)
        local.parent.mkdir(parents=True, exist_ok=True)

        # sprite exec ... cat REMOTE > local
        sprite_cmd = ["sprite", "exec"] + sprite_args + ["cat", remote_path]
        with open(local_path, "wb") as f:
            result = subprocess.run(sprite_cmd, stdout=f)
            if result.returncode != 0:
                print("Error: pull failed", file=sys.stderr)
                sys.exit(1)

    print("Done.")
