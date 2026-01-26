import child_process from "node:child_process";
import { closeSync, existsSync, openSync, readFileSync } from "node:fs";
import { mkdir, unlink } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";
// Vitest uses Vite's module resolution which doesn't support Node.js subpath
// imports (#cli/bins). We resolve the binary path directly instead.
import { bsc_exe, rescript_exe, runtimePath } from "./bins.mjs";
import { createClient } from "./grpc-client.mjs";

/**
 * Start a daemon process for the given project root.
 * Waits until the daemon is ready (socket path file exists and daemon responds to Ping).
 *
 * @param {string} projectRoot - Absolute path to the project root
 * @param {object} [options] - Optional configuration
 * @param {string} [options.otelEndpoint] - OTLP endpoint for OpenTelemetry (e.g., "http://127.0.0.1:4317")
 * @returns {Promise<{process: child_process.ChildProcess, socketPath: string, root: string}>}
 */
export async function startDaemon(projectRoot, options = {}) {
  // The daemon writes the actual socket path to this file
  const socketPathFile = join(projectRoot, "lib", "bs", "rescript.sock.path");

  // Ensure lib/bs directory exists
  await mkdir(join(projectRoot, "lib", "bs"), { recursive: true });

  const env = {
    ...process.env,
    RESCRIPT_BSC_EXE: bsc_exe,
    RESCRIPT_RUNTIME: runtimePath,
  };

  // Add OTLP endpoint if specified
  if (options.otelEndpoint) {
    env.OTEL_EXPORTER_OTLP_ENDPOINT = options.otelEndpoint;
  }

  // Open log files for stdout and stderr
  const daemonLogPath = join(projectRoot, "lib", "bs", "daemon.log");
  const daemonErrPath = join(projectRoot, "lib", "bs", "daemon.err");
  const outFd = openSync(daemonLogPath, "w");
  const errFd = openSync(daemonErrPath, "w");

  const proc = child_process.spawn(rescript_exe, ["daemon", projectRoot], {
    stdio: ["ignore", outFd, errFd],
    detached: true,
    env,
  });

  // Close the file descriptors in the parent process
  closeSync(outFd);
  closeSync(errFd);
  proc.unref();

  // Poll for socket path file + ping with 5s timeout
  const deadline = Date.now() + 5000;
  let lastError = null;
  let socketPath = null;
  while (Date.now() < deadline) {
    // Read the socket path from the file written by the daemon
    if (existsSync(socketPathFile)) {
      try {
        socketPath = readFileSync(socketPathFile, "utf-8").trim();
      } catch {
        // File might be in the process of being written
      }
    }

    if (socketPath && existsSync(socketPath)) {
      try {
        const client = createClient(socketPath);
        await client.Ping({});
        return { process: proc, socketPath, root: projectRoot };
      } catch (err) {
        // Connection refused is expected while daemon is still starting up
        if (err.code === 14 /* UNAVAILABLE */) {
          lastError = err;
        } else {
          throw err;
        }
      }
    }
    await sleep(100);
  }

  // If we get here, daemon failed to start
  try {
    proc.kill();
  } catch {}
  throw new Error(
    `Daemon failed to start within 5s at ${projectRoot}` +
      (lastError ? `: ${lastError.message}` : ""),
  );
}

/**
 * Stop a running daemon. Tries graceful Shutdown RPC first, falls back to SIGKILL.
 * Also cleans up the socket file.
 *
 * @param {{process: child_process.ChildProcess, socketPath: string, root: string} | null} daemon
 */
export async function stopDaemon(daemon) {
  if (!daemon) return;

  const pid = daemon.process.pid;

  // Try graceful shutdown via RPC
  try {
    const client = createClient(daemon.socketPath);
    await client.Shutdown({});
    await sleep(200);
  } catch {
    // Already dead or unreachable
  }

  // Check if process is still running and send SIGTERM
  if (isProcessRunning(pid)) {
    try {
      process.kill(pid, "SIGTERM");
      await sleep(200);
    } catch {
      // Already exited
    }
  }

  // If still running, force kill with SIGKILL
  if (isProcessRunning(pid)) {
    try {
      process.kill(pid, "SIGKILL");
      await sleep(100);
    } catch {
      // Already exited
    }
  }

  // Clean up the socket file (now in /tmp, not in the sandbox)
  try {
    await unlink(daemon.socketPath);
  } catch {
    // Socket might already be cleaned up by daemon
  }
}

/**
 * Check if a process is still running.
 * @param {number} pid
 * @returns {boolean}
 */
function isProcessRunning(pid) {
  try {
    // Sending signal 0 checks if the process exists without actually sending a signal
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}
