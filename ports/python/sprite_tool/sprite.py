"""Subprocess wrapper for the sprite CLI."""

import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import List, Optional


def find_sprite_cli() -> Optional[str]:
    """Find the sprite CLI binary on PATH."""
    return shutil.which("sprite")


def run(
    args: List[str],
    check: bool = True,
    capture_output: bool = False,
    stdin_data: Optional[bytes] = None,
    stdin_file: Optional[str] = None,
    stdout_file: Optional[str] = None,
) -> subprocess.CompletedProcess:
    """Run a sprite CLI command.

    Args:
        args: Command arguments (without leading 'sprite').
        check: Raise on non-zero exit.
        capture_output: Capture stdout/stderr.
        stdin_data: Bytes to feed to stdin.
        stdin_file: Path to file to open as stdin.
        stdout_file: Path to file to open as stdout.
    """
    cmd = ["sprite"] + args

    stdin_handle = None
    stdout_handle = None
    opened_files = []

    try:
        if stdin_data is not None:
            stdin_handle = subprocess.PIPE
        elif stdin_file is not None:
            fh = open(stdin_file, "rb")
            opened_files.append(fh)
            stdin_handle = fh

        if stdout_file is not None:
            fh = open(stdout_file, "wb")
            opened_files.append(fh)
            stdout_handle = fh
        elif capture_output:
            stdout_handle = subprocess.PIPE

        result = subprocess.run(
            cmd,
            stdin=stdin_handle,
            stdout=stdout_handle,
            stderr=subprocess.PIPE if capture_output else None,
            input=stdin_data if stdin_data is not None else None,
            check=check,
        )
        return result
    finally:
        for fh in opened_files:
            fh.close()


def sx(sprite_name: str, cmd: str, dry_run: bool = False) -> Optional[str]:
    """Run a command inside a sprite via bash.

    Equivalent to: sprite exec -s SPRITE bash -c "CMD"

    Args:
        sprite_name: The sprite to exec on.
        cmd: The bash command string to execute.
        dry_run: If True, just print the command.

    Returns:
        stdout as string if capture is needed, else None.
    """
    if dry_run:
        print(f'  [dry-run] sprite exec -s {sprite_name} bash -c "{cmd}"')
        return None

    result = subprocess.run(
        ["sprite", "exec", "-s", sprite_name, "bash", "-c", cmd],
        capture_output=True,
        text=True,
    )
    return result.stdout.strip() if result.stdout else ""


def sx_passthrough(sprite_name: str, cmd: str, dry_run: bool = False) -> int:
    """Run a command inside a sprite, passing stdout/stderr through to terminal.

    Returns the exit code.
    """
    if dry_run:
        print(f'  [dry-run] sprite exec -s {sprite_name} bash -c "{cmd}"')
        return 0

    result = subprocess.run(
        ["sprite", "exec", "-s", sprite_name, "bash", "-c", cmd],
    )
    return result.returncode


def push_file(sprite_name: str, src: str, dest: str, dry_run: bool = False) -> None:
    """Push a local file to a sprite.

    Equivalent to:
        sprite exec -s SPRITE bash -c "mkdir -p $(dirname DEST)"
        sprite exec -s SPRITE bash -c "cat > DEST" < src
    """
    if dry_run:
        print(f"  [dry-run] push {src} -> sprite:{dest}")
        return

    dest_dir = str(Path(dest).parent)
    subprocess.run(
        ["sprite", "exec", "-s", sprite_name, "bash", "-c", f"mkdir -p '{dest_dir}'"],
        check=True,
    )
    with open(src, "rb") as f:
        subprocess.run(
            ["sprite", "exec", "-s", sprite_name, "bash", "-c", f"cat > '{dest}'"],
            stdin=f,
            check=True,
        )


def push_dir(sprite_name: str, src: str, dest: str, dry_run: bool = False) -> None:
    """Push a local directory to a sprite via tar.

    Equivalent to:
        sprite exec -s SPRITE bash -c "mkdir -p DEST"
        tar czf - -C parent base | sprite exec -s SPRITE bash -c "tar xzf - -C parent_of_dest"
    """
    if dry_run:
        print(f"  [dry-run] push dir {src} -> sprite:{dest}")
        return

    src_path = Path(src)
    parent = str(src_path.parent)
    base = src_path.name
    dest_parent = str(Path(dest).parent)

    subprocess.run(
        ["sprite", "exec", "-s", sprite_name, "bash", "-c", f"mkdir -p '{dest}'"],
        check=True,
    )

    # tar czf - -C parent base | sprite exec -s SPRITE bash -c "tar xzf - -C dest_parent"
    tar_proc = subprocess.Popen(
        ["tar", "czf", "-", "-C", parent, base],
        stdout=subprocess.PIPE,
    )
    sprite_proc = subprocess.Popen(
        [
            "sprite", "exec", "-s", sprite_name, "bash", "-c",
            f"tar xzf - -C '{dest_parent}'",
        ],
        stdin=tar_proc.stdout,
    )
    tar_proc.stdout.close()  # Allow tar_proc to receive SIGPIPE if sprite_proc exits
    sprite_proc.communicate()
    tar_proc.wait()

    if sprite_proc.returncode != 0:
        raise subprocess.CalledProcessError(sprite_proc.returncode, "sprite exec")


def sprite_list(capture: bool = True) -> str:
    """Run 'sprite ls' and return stdout."""
    result = subprocess.run(
        ["sprite", "ls"],
        capture_output=True,
        text=True,
    )
    return result.stdout if result.stdout else ""


def sprite_exists(sprite_name: str) -> bool:
    """Check if a sprite with the given name already exists."""
    output = sprite_list()
    for line in output.splitlines():
        # Check for the sprite name as a whole word in the listing
        parts = line.split()
        if sprite_name in parts:
            return True
    return False
