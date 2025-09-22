#!/usr/bin/env node

// @ts-check

import * as child_process from "node:child_process";
import { rescript_exe } from "./common/bins.js";
import { runtimePath } from "./common/runtime.js";

const args = process.argv.slice(2);

// We intentionally use spawn (async) instead of execFileSync (sync) here.
// Rationale:
// - execFileSync blocks Node's event loop, so Ctrl+C (SIGINT) causes Node to
//   exit immediately without giving us a chance to forward the signal to the
//   child and wait for its cleanup. In watch mode, the Rust watcher prints
//   "Exiting..." on SIGINT and performs cleanup; with execFileSync that output
//   may appear after the shell prompt and sometimes requires an extra keypress.
// - spawn lets us install signal handlers, forward them to the child, and then
//   exit the parent with the correct status only after the child has exited.
const child = child_process.spawn(rescript_exe, args, {
  stdio: "inherit",
  env: { ...process.env, RESCRIPT_RUNTIME: runtimePath },
});

// Map POSIX signal names to conventional exit status numbers so we can
// reproduce the usual 128 + signal behavior when exiting due to a signal.
/** @type {Record<string, number>} */
const signalToNumber = { SIGINT: 2, SIGTERM: 15, SIGHUP: 1, SIGQUIT: 3 };

let forwardedSignal = false;
/**
 * @param {NodeJS.Signals} signal
 */
const handleSignal = signal => {
  // Intercept the signal in the parent, forward it to the child, and let the
  // child perform its own cleanup. This ensures ordered shutdown in watch mode.
  // Guard against double-forwarding since terminals or OSes can deliver
  // multiple signals (e.g., repeated Ctrl+C).
  // Prevent Node from exiting immediately; forward to child first
  if (forwardedSignal) return;
  forwardedSignal = true;
  try {
    if (child.exitCode === null && child.signalCode == null) {
      child.kill(signal);
    }
  } catch {
    // best effort
  }
};

process.on("SIGINT", handleSignal);
process.on("SIGTERM", handleSignal);
process.on("SIGHUP", handleSignal);
process.on("SIGQUIT", handleSignal);

// Cross-platform note:
// - On Unix, Ctrl+C sends SIGINT to the process group; we also explicitly
//   forward it to the child to be robust.
// - On Windows, Node maps kill('SIGINT'/'SIGTERM') to console control events;
//   the Rust watcher (via the ctrlc crate) handles these and exits cleanly.

// Ensure no orphaned process if parent exits unexpectedly
process.on("exit", () => {
  if (child.exitCode === null && child.signalCode == null) {
    try {
      child.kill("SIGTERM");
    } catch {
      // ignore
    }
  }
});

child.on("exit", (code, signal) => {
  process.removeListener("SIGINT", handleSignal);
  process.removeListener("SIGTERM", handleSignal);
  process.removeListener("SIGHUP", handleSignal);
  process.removeListener("SIGQUIT", handleSignal);

  // If the child exited due to a signal, emulate the conventional exit status
  // (128 + signalNumber). Otherwise, pass through the child's numeric exit code.
  if (signal) {
    const n = signalToNumber[signal];
    process.exit(typeof n === "number" ? 128 + n : 1);
  } else {
    process.exit(typeof code === "number" ? code : 0);
  }
});

// Surface spawn errors (e.g., executable not found) and exit with failure.
child.on("error", err => {
  console.error(err?.message ?? String(err));
  process.exit(1);
});
