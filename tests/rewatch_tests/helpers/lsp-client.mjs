import child_process from "node:child_process";
import {
  createProtocolConnection,
  ExitNotification,
  InitializedNotification,
  InitializeRequest,
  ShutdownRequest,
} from "vscode-languageserver-protocol/node.js";
import { bsc_exe, rescript_exe, runtimePath } from "./bins.mjs";

/**
 * Create an LSP client that communicates via the vscode-languageserver-protocol.
 *
 * @param {string} cwd - Working directory for the LSP server process
 * @param {string} [otelEndpoint] - Optional OTEL endpoint for telemetry
 */
export function createLspClient(cwd, otelEndpoint) {
  const env = {
    ...process.env,
    RESCRIPT_BSC_EXE: bsc_exe,
    RESCRIPT_RUNTIME: runtimePath,
  };

  if (otelEndpoint) {
    env.OTEL_EXPORTER_OTLP_ENDPOINT = otelEndpoint;
  }

  const proc = child_process.spawn(rescript_exe, ["lsp"], {
    cwd,
    stdio: ["pipe", "pipe", "pipe"],
    env,
  });

  // Collect stderr for debugging CI failures
  let stderrAll = "";
  proc.stderr.on("data", chunk => {
    stderrAll += chunk.toString();
  });

  // Track if process has exited
  let exited = false;
  let exitCode = null;
  proc.once("exit", code => {
    exited = true;
    exitCode = code;
    // If the process exits unexpectedly, close the connection so pending
    // requests reject immediately instead of hanging until timeout.
    if (code !== 0) {
      connection.dispose();
    }
  });

  proc.on("error", err => {
    if (err.name !== "AbortError") {
      throw err;
    }
  });

  const connection = createProtocolConnection(proc.stdout, proc.stdin);
  connection.onError(([error]) => {
    console.error("[lsp-client] Connection error:", error);
  });
  connection.onClose(() => {
    if (!exited) {
      console.error("[lsp-client] Connection closed unexpectedly");
    }
  });
  connection.listen();

  /**
   * Send a request with a timeout. If the server doesn't respond in time
   * (e.g. due to a panic), reject with a descriptive error including stderr.
   */
  function sendRequest(type, params, timeoutMs = 3000) {
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        reject(
          new Error(
            `LSP request "${type.method}" timed out after ${timeoutMs}ms.\n\nServer stderr:\n${stderrAll}`,
          ),
        );
      }, timeoutMs);

      connection.sendRequest(type, params).then(
        result => {
          clearTimeout(timer);
          resolve(result);
        },
        err => {
          clearTimeout(timer);
          reject(err);
        },
      );
    });
  }

  return {
    /**
     * Send initialize request and initialized notification.
     * @param {string} rootUri - The root URI of the workspace
     * @returns {Promise<import("vscode-languageserver-protocol").InitializeResult>}
     */
    async initialize(rootUri) {
      const result = await sendRequest(InitializeRequest.type, {
        processId: process.pid,
        rootUri,
        capabilities: {},
      });
      connection.sendNotification(InitializedNotification.type, {});
      return result;
    },

    /**
     * Send shutdown request and exit notification, then wait for process exit.
     * @returns {Promise<void>}
     */
    async shutdown() {
      if (exited) return;
      await sendRequest(ShutdownRequest.type, undefined);
      connection.sendNotification(ExitNotification.type);

      // Give the exit notification time to be written before closing
      await new Promise(r => setTimeout(r, 100));
      connection.dispose();
      proc.stdin.end();

      // Wait for process to exit
      const deadline = Date.now() + 5000;
      while (!exited && Date.now() < deadline) {
        await new Promise(r => setTimeout(r, 50));
      }

      if (!exited) {
        proc.kill("SIGTERM");
      }
    },

    /** The exit code (null if still running). */
    get exitCode() {
      return exitCode;
    },

    /** All stderr output from the LSP server process. */
    getStderr() {
      return stderrAll;
    },

    /** The underlying JSON-RPC connection. */
    connection,

    /** The underlying child process. */
    process: proc,
  };
}
