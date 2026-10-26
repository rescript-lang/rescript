"""Process-level tests for the shared-artifact lock prototype."""

import json
import os
from pathlib import Path
import selectors
import signal
import subprocess
import sys
import tempfile
import unittest

WRAPPER = Path(__file__).resolve().with_name("with_test_lock.py")
HOLD = """
import pathlib, sys, time
print('START ' + sys.argv[1], flush=True)
while not pathlib.Path(sys.argv[2]).exists():
    time.sleep(0.02)
print('END ' + sys.argv[1], flush=True)
"""


@unittest.skipUnless(os.name == "posix", "prototype uses POSIX flock")
class TestArtifactLock(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="rescript-lock-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.children = []
        self.addCleanup(self.stop_children)

    def stop_children(self):
        for child in self.children:
            if child.poll() is None:
                child.kill()
            child.wait(timeout=5)
            child.stdout.close()
            child.stderr.close()

    def command(self, root, label, *command):
        return [sys.executable, str(WRAPPER), "--root", str(root),
                "--label", label, "--", *command]

    def start(self, root, label, *command, env=None):
        child = subprocess.Popen(self.command(root, label, *command),
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 text=True, env=env)
        self.children.append(child)
        return child

    def line(self, stream):
        with selectors.DefaultSelector() as selector:
            selector.register(stream, selectors.EVENT_READ)
            self.assertTrue(selector.select(timeout=5), "timed out waiting for output")
        return stream.readline().strip()

    def hold(self, root, label):
        release = root / (label + ".release")
        child = self.start(root, label, sys.executable, "-c", HOLD, label, str(release))
        return child, release

    def test_competing_suites_wait_for_entire_command(self):
        first, release_first = self.hold(self.root, "test")
        self.assertIn("acquired", self.line(first.stderr))
        self.assertEqual(self.line(first.stdout), "START test")
        second, release_second = self.hold(self.root, "test-analysis")
        self.assertIn("waiting for shared artifacts", self.line(second.stderr))
        self.assertIsNone(second.poll())
        release_first.touch()
        self.assertEqual(self.line(first.stdout), "END test")
        self.assertEqual(first.wait(timeout=5), 0)
        self.assertIn("acquired", self.line(second.stderr))
        self.assertEqual(self.line(second.stdout), "START test-analysis")
        release_second.touch()
        self.assertEqual(self.line(second.stdout), "END test-analysis")
        self.assertEqual(second.wait(timeout=5), 0)

    def test_node_command_retains_the_kernel_lock(self):
        release = self.root / "node.release"
        code = ("console.log('START node'); setInterval(() => {"
                "if (require('node:fs').existsSync(process.argv[1])) process.exit(0);"
                "}, 20)")
        first = self.start(self.root, "node", "node", "--input-type=commonjs",
                           "-e", code, str(release))
        self.assertEqual(self.line(first.stdout), "START node")
        second = self.start(self.root, "next", sys.executable, "-c", "pass")
        self.assertIn("waiting", self.line(second.stderr))
        release.touch()
        self.assertEqual(first.wait(timeout=5), 0)
        _, err = second.communicate(timeout=5)
        self.assertEqual(second.returncode, 0, err)

    def test_nested_command_does_not_deadlock(self):
        nested = self.command(self.root, "nested", sys.executable, "-c", "print('nested ok')")
        code = "import subprocess,sys; sys.exit(subprocess.call(" + repr(nested) + "))"
        child = self.start(self.root, "outer", sys.executable, "-c", code)
        out, err = child.communicate(timeout=5)
        self.assertEqual(child.returncode, 0, err)
        self.assertEqual(out.strip(), "nested ok")
        self.assertEqual(err.count("acquired"), 1)

    def test_failure_releases_lock_and_preserves_exit_status(self):
        child = self.start(self.root, "fails", sys.executable, "-c", "raise SystemExit(7)")
        child.communicate(timeout=5)
        self.assertEqual(child.returncode, 7)
        successor = self.start(self.root, "next", sys.executable, "-c", "pass")
        _, err = successor.communicate(timeout=5)
        self.assertEqual(successor.returncode, 0, err)
        self.assertNotIn("waiting", err)

    def test_killed_owner_releases_kernel_lock(self):
        first, _ = self.hold(self.root, "killed")
        self.assertIn("acquired", self.line(first.stderr))
        self.assertEqual(self.line(first.stdout), "START killed")
        second = self.start(self.root, "next", sys.executable, "-c", "pass")
        self.assertIn("waiting", self.line(second.stderr))
        first.kill()
        self.assertEqual(first.wait(timeout=5), -signal.SIGKILL)
        _, err = second.communicate(timeout=5)
        self.assertEqual(second.returncode, 0, err)
        self.assertIn("acquired", err)

    def test_separate_checkouts_do_not_block_each_other(self):
        first, release_first = self.hold(self.root, "first")
        self.assertEqual(self.line(first.stdout), "START first")
        other_root = self.root / "other"
        other_root.mkdir()
        second, release_second = self.hold(other_root, "second")
        self.assertIn("acquired", self.line(second.stderr))
        self.assertEqual(self.line(second.stdout), "START second")
        self.assertIsNone(first.poll())
        release_first.touch()
        release_second.touch()

    def test_missing_flock_fails_instead_of_running_unlocked(self):
        shadow = Path(tempfile.mkdtemp(dir=self.root))
        (shadow / "fcntl.py").write_text('raise ImportError("no flock here")')
        env = {**os.environ, "PYTHONPATH": str(shadow)}
        child = self.start(self.root, "no-flock", sys.executable, "-c", "print('ran')",
                           env=env)
        out, err = child.communicate(timeout=5)
        self.assertEqual(child.returncode, 2, err)
        self.assertNotIn("ran", out)
        self.assertIn("requires POSIX flock", err)

    def test_stale_environment_marker_does_not_skip_lock(self):
        first, release_first = self.hold(self.root, "owner")
        self.assertEqual(self.line(first.stdout), "START owner")
        stale = {"root": str(self.root), "pid": os.getpid(), "token": "old"}
        env = {**os.environ, "RESCRIPT_TEST_LOCK": json.dumps(stale)}
        second = self.start(self.root, "stale", sys.executable, "-c", "pass", env=env)
        self.assertIn("waiting", self.line(second.stderr))
        release_first.touch()
        _, err = second.communicate(timeout=5)
        self.assertEqual(second.returncode, 0, err)


if __name__ == "__main__":
    unittest.main(verbosity=2)
