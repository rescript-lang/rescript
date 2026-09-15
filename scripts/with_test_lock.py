#!/usr/bin/env python3
"""Run a command under the checkout's shared-test-artifact lock (POSIX)."""

import argparse
import json
import os
from pathlib import Path
import sys
import uuid

LOCK_ENV = "RESCRIPT_TEST_LOCK"
LOCK_NAME = ".rescript-test.lock"


def inherited_lock(root):
    """Recognize nested commands, without trusting a stale environment marker."""
    try:
        owner = json.loads(os.environ.get(LOCK_ENV, "null"))
        if not isinstance(owner, dict) or owner.get("root") != str(root):
            return False
        recorded = json.loads((root / LOCK_NAME).read_text())
        if recorded != owner:
            return False
        os.kill(owner["pid"], 0)
        return True
    except (OSError, ValueError, KeyError, TypeError):
        return False


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parent.parent)
    parser.add_argument("--label", default="test command")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.command
    if command[:1] == ["--"]:
        command = command[1:]
    if not command:
        parser.error("expected a command after --")
    root = args.root.resolve()
    if inherited_lock(root):
        os.execvpe(command[0], command, os.environ)

    try:
        import fcntl
    except ImportError:
        # Windows has no flock, and CI runs these commands there. Degrade to an
        # unlocked run instead of failing a supported platform. Anywhere else a
        # missing flock is unexpected, so fail rather than silently race.
        if sys.platform == "win32":
            print(f"[test-lock] {args.label}: unlocked (no flock on this platform)",
                  file=sys.stderr, flush=True)
            os.execvpe(command[0], command, os.environ)
        parser.error("this lock prototype requires POSIX flock (macOS/Linux)")

    # Never unlink this file: waiters must all lock the same inode. Keeping it
    # outside build directories also prevents ordinary test cleanup removing it.
    fd = os.open(root / LOCK_NAME, os.O_CREAT | os.O_RDWR, 0o600)
    try:
        try:
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            print(f"[test-lock] {args.label}: waiting for shared artifacts in {root}",
                  file=sys.stderr, flush=True)
            fcntl.flock(fd, fcntl.LOCK_EX)

        owner = {"root": str(root), "pid": os.getpid(), "token": uuid.uuid4().hex}
        encoded = json.dumps(owner)
        os.ftruncate(fd, 0)
        os.write(fd, encoded.encode())
        os.set_inheritable(fd, True)
        env = {**os.environ, LOCK_ENV: encoded}
        print(f"[test-lock] {args.label}: acquired", file=sys.stderr, flush=True)
        # exec preserves PID, signals and exit status. The inheritable descriptor
        # holds the kernel lock for the command's lifetime; even SIGKILL releases
        # it once processes inheriting that descriptor have exited.
        os.execvpe(command[0], command, env)
    finally:
        os.close(fd)


if __name__ == "__main__":
    main()
