import child_process from "node:child_process";
import {
  createProtocolConnection,
  ExitNotification,
  InitializedNotification,
  InitializeRequest,
  RegistrationRequest,
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

  // Notification collection and waiting infrastructure.
  // Listeners are checked in order: pending waiters first, then stored for later.
  /** @type {Map<string, Array<{params: any}>>} */
  const notifications = new Map();
  /** @type {Array<{method: string, resolve: (params: any) => void}>} */
  const notificationWaiters = [];

  function onNotification(method, params) {
    // Check if anyone is waiting for this notification
    const idx = notificationWaiters.findIndex(w => w.method === method);
    if (idx !== -1) {
      const [waiter] = notificationWaiters.splice(idx, 1);
      waiter.resolve(params);
      return;
    }
    // Otherwise store for later retrieval
    if (!notifications.has(method)) {
      notifications.set(method, []);
    }
    notifications.get(method).push({ params });
  }

  // Collect server-to-client registration requests.
  /** @type {import("vscode-languageserver-protocol").Registration[]} */
  const registrations = [];
  connection.onRequest(RegistrationRequest.type, params => {
    registrations.push(...params.registrations);
    onNotification("client/registerCapability", params);
    return undefined;
  });

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
        workspaceFolders: [{ uri: rootUri, name: "root" }],
      });
      // Start waiting for the registration before sending initialized,
      // so we don't miss it if the server responds immediately.
      const registrationDone = this.waitForNotification(
        "client/registerCapability",
      );
      connection.sendNotification(InitializedNotification.type, {});
      await registrationDone;
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

    /** Registrations received from the server via client/registerCapability. */
    get registrations() {
      return registrations;
    },

    /**
     * Wait for a notification from the server.
     * If a matching notification already arrived, resolves immediately.
     * @param {string} method - The notification method to wait for
     * @param {number} [timeoutMs=5000] - Timeout in milliseconds
     * @returns {Promise<any>} The notification params
     */
    waitForNotification(method, timeoutMs = 5000) {
      // Check if we already have one stored
      const stored = notifications.get(method);
      if (stored && stored.length > 0) {
        const { params } = stored.shift();
        return Promise.resolve(params);
      }

      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          const idx = notificationWaiters.findIndex(w => w.resolve === resolve);
          if (idx !== -1) notificationWaiters.splice(idx, 1);
          reject(
            new Error(
              `Timeout waiting for notification "${method}" after ${timeoutMs}ms.\n\nServer stderr:\n${stderrAll}`,
            ),
          );
        }, timeoutMs);

        notificationWaiters.push({
          method,
          resolve: params => {
            clearTimeout(timer);
            resolve(params);
          },
        });
      });
    },

    /**
     * Get all collected notifications for a method (non-blocking).
     * @param {string} method
     * @returns {Array<{params: any}>}
     */
    getNotifications(method) {
      return notifications.get(method) ?? [];
    },

    /** The underlying JSON-RPC connection. */
    connection,

    /** The underlying child process. */
    process: proc,
  };
}
