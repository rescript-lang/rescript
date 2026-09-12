// @ts-check

import * as child_process from "node:child_process";
import { runtimePath } from "./runtime.js";

/** @type {Record<string, number>} */
const signalToNumber = { SIGINT: 2, SIGTERM: 15, SIGHUP: 1, SIGQUIT: 3 };

/**
 * Run a build-system executable with the package runtime and forward terminal
 * signals so watch mode can clean up before the Node launcher exits.
 *
 * @param {string} executable
 */
export function runBuildSystem(executable) {
  const child = child_process.spawn(executable, process.argv.slice(2), {
    stdio: "inherit",
    env: { ...process.env, RESCRIPT_RUNTIME: runtimePath },
  });

  let forwardedSignal = false;
  /** @param {NodeJS.Signals} signal */
  const handleSignal = signal => {
    if (forwardedSignal) return;
    forwardedSignal = true;
    // Ctrl+C is delivered to every process attached to the Windows console.
    // child.kill("SIGINT") uses TerminateProcess there and can kill the build
    // system before its console handler releases locks and owned descendants.
    if (process.platform === "win32" && signal === "SIGINT") return;
    try {
      if (child.exitCode === null && child.signalCode == null) {
        child.kill(signal);
      }
    } catch {
      // Signal forwarding is best effort if the child exited concurrently.
    }
  };

  process.on("SIGINT", handleSignal);
  process.on("SIGTERM", handleSignal);
  process.on("SIGHUP", handleSignal);
  process.on("SIGQUIT", handleSignal);

  process.on("exit", () => {
    if (child.exitCode === null && child.signalCode == null) {
      try {
        child.kill("SIGTERM");
      } catch {
        // The child may already have exited.
      }
    }
  });

  child.on("exit", (code, signal) => {
    process.removeListener("SIGINT", handleSignal);
    process.removeListener("SIGTERM", handleSignal);
    process.removeListener("SIGHUP", handleSignal);
    process.removeListener("SIGQUIT", handleSignal);

    if (signal) {
      const number = signalToNumber[signal];
      process.exit(typeof number === "number" ? 128 + number : 1);
    } else {
      process.exit(typeof code === "number" ? code : 0);
    }
  });

  child.on("error", error => {
    console.error(error?.message ?? String(error));
    process.exit(1);
  });
}
