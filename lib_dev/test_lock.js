// @ts-check

import { spawn } from "node:child_process";
import { readFileSync, realpathSync } from "node:fs";
import { constants } from "node:os";
import { fileURLToPath } from "node:url";

const root = realpathSync(fileURLToPath(new URL("../", import.meta.url)));
const wrapper = fileURLToPath(
  new URL("../scripts/with_test_lock.py", import.meta.url),
);

/** Acquire the checkout lock before a directly invoked runner does any work. */
export async function ensureTestLock() {
  // Windows has no flock. CI runs this runner there, so proceed unlocked
  // rather than failing; concurrent suites in one checkout stay unsupported.
  if (process.platform === "win32") return;
  try {
    const owner = JSON.parse(process.env.RESCRIPT_TEST_LOCK ?? "null");
    const recorded = JSON.parse(
      readFileSync(`${root}/.rescript-test.lock`, "utf8"),
    );
    if (
      owner?.root === root &&
      owner.pid === recorded.pid &&
      owner.token === recorded.token
    ) {
      process.kill(owner.pid, 0);
      return;
    }
  } catch {
    // Missing/stale ownership: acquire through the OS lock, never skip it.
  }
  const child = spawn(
    "python3",
    [
      wrapper,
      "--label",
      "scripts/test.js",
      "--",
      process.execPath,
      ...process.argv.slice(1),
    ],
    { stdio: "inherit" },
  );
  /** @type {NodeJS.Signals[]} */
  const signals = ["SIGINT", "SIGTERM", "SIGHUP"];
  const forwards = signals.map(signal => {
    const forward = () => {
      child.kill(signal);
    };
    process.on(signal, forward);
    return { signal, forward };
  });
  let status;
  try {
    status = await new Promise(resolve => {
      child.once("error", error => {
        console.error(`[test-lock] ${error.message}`);
        resolve(1);
      });
      child.once("exit", (code, signal) =>
        resolve(code ?? (signal ? 128 + constants.signals[signal] : 1)),
      );
    });
  } finally {
    for (const { signal, forward } of forwards)
      process.removeListener(signal, forward);
  }
  process.exit(status);
}
