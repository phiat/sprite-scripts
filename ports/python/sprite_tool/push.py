"""sprite-tool push: Push local file or directory to a sprite."""

import subprocess
import sys
from pathlib import Path


def _usage():
    print("""\
Usage: sprite-tool push <local-path> <remote-path> [sprite-name]

Examples:
  sprite-tool push ./file.txt /home/sprite/file.txt
  sprite-tool push ./mydir /home/sprite/mydir
  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk""")
    sys.exit(1)


def run(args):
    """Execute the push subcommand."""
    if len(args) < 2:
        _usage()

    local_path = args[0]
    remote_path = args[1]
    sprite_name = args[2] if len(args) > 2 else None

    sprite_args = []
    if sprite_name:
        sprite_args = ["-s", sprite_name]

    local = Path(local_path)
    if not local.exists():
        print(f"Error: {local_path} does not exist", file=sys.stderr)
        sys.exit(1)

    if local.is_dir():
        print(f"Pushing directory: {local_path} -> {remote_path}")
        parent = str(local.parent)
        base = local.name

        # tar czf - -C parent base | sprite exec ... bash -c "mkdir -p DEST && tar xzf - -C DEST --strip-components=1"
        tar_proc = subprocess.Popen(
            ["tar", "czf", "-", "-C", parent, base],
            stdout=subprocess.PIPE,
        )
        sprite_cmd = (
            ["sprite", "exec"]
            + sprite_args
            + [
                "bash",
                "-c",
                f"mkdir -p '{remote_path}' && tar xzf - -C '{remote_path}' --strip-components=1",
            ]
        )
        sprite_proc = subprocess.Popen(
            sprite_cmd,
            stdin=tar_proc.stdout,
        )
        tar_proc.stdout.close()
        sprite_proc.communicate()
        tar_proc.wait()

        if sprite_proc.returncode != 0:
            print("Error: push failed", file=sys.stderr)
            sys.exit(1)
    else:
        print(f"Pushing file: {local_path} -> {remote_path}")
        remote_dir = str(Path(remote_path).parent)

        sprite_cmd = (
            ["sprite", "exec"]
            + sprite_args
            + [
                "bash",
                "-c",
                f"mkdir -p '{remote_dir}' && cat > '{remote_path}'",
            ]
        )
        with open(local_path, "rb") as f:
            result = subprocess.run(sprite_cmd, stdin=f)
            if result.returncode != 0:
                print("Error: push failed", file=sys.stderr)
                sys.exit(1)

    print("Done.")
