import child_process from "node:child_process";
import { readFileSync, realpathSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import {
  CompletionRequest,
  CompletionResolveRequest,
  createProtocolConnection,
  DefinitionRequest,
  DidChangeTextDocumentNotification,
  DidCloseTextDocumentNotification,
  DidOpenTextDocumentNotification,
  DidSaveTextDocumentNotification,
  DocumentFormattingRequest,
  DocumentSymbolRequest,
  ExitNotification,
  HoverRequest,
  InitializedNotification,
  InitializeRequest,
  PrepareRenameRequest,
  PublishDiagnosticsNotification,
  ReferencesRequest,
  RegistrationRequest,
  RenameRequest,
  ShutdownRequest,
  SignatureHelpRequest,
  TypeDefinitionRequest,
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

  // Collect publishDiagnostics notifications keyed by sandbox-relative path.
  // Each new notification for a path replaces the previous one (same as LSP semantics).
  // URIs are normalized to relative paths so tests don't depend on absolute sandbox paths.
  const cwdUri = pathToFileURL(realpathSync(cwd)).href;
  /** @type {Map<string, import("vscode-languageserver-protocol").Diagnostic[]>} */
  const diagnosticsByPath = new Map();

  connection.onNotification(PublishDiagnosticsNotification.type, params => {
    const relativePath = params.uri.startsWith(cwdUri + "/")
      ? params.uri.slice(cwdUri.length + 1)
      : params.uri;
    diagnosticsByPath.set(relativePath, params.diagnostics);
    onNotification("textDocument/publishDiagnostics", params);
  });

  // Catch-all for custom notifications (e.g. rescript/buildFinished).
  connection.onNotification((method, params) => {
    onNotification(method, params);
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

  // Track opened files to enforce open-before-edit ordering
  /** @type {Set<string>} */
  const openedFiles = new Set();

  function toUri(relativePath) {
    return pathToFileURL(path.join(realpathSync(cwd), relativePath)).href;
  }

  function assertOpen(relativePath, uri) {
    if (!openedFiles.has(uri)) {
      throw new Error(`"${relativePath}" was not opened. Call openFile first.`);
    }
  }

  // Version counter for didChange notifications
  let nextVersion = 1;

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

    /**
     * Notify the LSP server that a file was saved.
     * @param {string} relativePath - Path relative to the sandbox root
     */
    saveFile(relativePath) {
      const uri = toUri(relativePath);
      connection.sendNotification(DidSaveTextDocumentNotification.type, {
        textDocument: { uri },
      });
    },

    /**
     * Notify the LSP server that a file's content changed (unsaved edit).
     * Sends full document content (TextDocumentSyncKind.Full).
     * @param {string} relativePath - Path relative to the sandbox root
     * @param {string} content - The full updated file content
     */
    editFile(relativePath, content) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      connection.sendNotification(DidChangeTextDocumentNotification.type, {
        textDocument: { uri, version: nextVersion++ },
        contentChanges: [{ text: content }],
      });
    },

    /**
     * Send a completion request for a position in a file.
     * @param {string} relativePath - Path relative to the sandbox root
     * @param {number} line - Zero-based line number
     * @param {number} character - Zero-based character offset
     * @returns {Promise<import("vscode-languageserver-protocol").CompletionItem[]>}
     */
    async completeFor(relativePath, line, character) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      const result = await sendRequest(CompletionRequest.type, {
        textDocument: { uri },
        position: { line, character },
      });
      if (!result) return [];
      return Array.isArray(result) ? result : result.items;
    },

    /**
     * Send a completionItem/resolve request to enrich a completion item.
     * @param {import("vscode-languageserver-protocol").CompletionItem} item - The completion item to resolve
     * @returns {Promise<import("vscode-languageserver-protocol").CompletionItem>}
     */
    async resolveCompletion(item) {
      return sendRequest(CompletionResolveRequest.type, item);
    },

    /**
     * Send a hover request for a position in a file.
     * @param {string} relativePath - Path relative to the sandbox root
     * @param {number} line - Zero-based line number
     * @param {number} character - Zero-based character offset
     * @returns {Promise<import("vscode-languageserver-protocol").Hover | null>}
     */
    async hoverFor(relativePath, line, character) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      return sendRequest(HoverRequest.type, {
        textDocument: { uri },
        position: { line, character },
      });
    },

    /**
     * Send a definition request for a position in a file.
     * @param {string} relativePath - Path relative to the sandbox root
     * @param {number} line - Zero-based line number
     * @param {number} character - Zero-based character offset
     * @returns {Promise<import("vscode-languageserver-protocol").Location | null>}
     */
    async definitionFor(relativePath, line, character) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      return sendRequest(DefinitionRequest.type, {
        textDocument: { uri },
        position: { line, character },
      });
    },

    /**
     * Send a type definition request for a position in a file.
     * @param {string} relativePath - Path relative to the sandbox root
     * @param {number} line - Zero-based line number
     * @param {number} character - Zero-based character offset
     * @returns {Promise<import("vscode-languageserver-protocol").Location | null>}
     */
    async typeDefinitionFor(relativePath, line, character) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      return sendRequest(TypeDefinitionRequest.type, {
        textDocument: { uri },
        position: { line, character },
      });
    },

    /**
     * Send a references request for a position in a file.
     * @param {string} relativePath - Path relative to the sandbox root
     * @param {number} line - Zero-based line number
     * @param {number} character - Zero-based character offset
     * @returns {Promise<import("vscode-languageserver-protocol").Location[] | null>}
     */
    async referencesFor(relativePath, line, character) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      return sendRequest(ReferencesRequest.type, {
        textDocument: { uri },
        position: { line, character },
        context: { includeDeclaration: true },
      });
    },

    /**
     * Request document symbols for an open file.
     * @param {string} relativePath - Path relative to the sandbox root
     */
    async documentSymbolsFor(relativePath) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      return sendRequest(DocumentSymbolRequest.type, {
        textDocument: { uri },
      });
    },

    /**
     * Send a prepareRename request for a position in a file.
     * @param {string} relativePath - Path relative to the sandbox root
     * @param {number} line - Zero-based line number
     * @param {number} character - Zero-based character offset
     * @returns {Promise<import("vscode-languageserver-protocol").PrepareRenameResult | null>}
     */
    async prepareRenameFor(relativePath, line, character) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      return sendRequest(PrepareRenameRequest.type, {
        textDocument: { uri },
        position: { line, character },
      });
    },

    /**
     * Send a rename request for a position in a file.
     * @param {string} relativePath - Path relative to the sandbox root
     * @param {number} line - Zero-based line number
     * @param {number} character - Zero-based character offset
     * @param {string} newName - The new name for the symbol
     * @returns {Promise<import("vscode-languageserver-protocol").WorkspaceEdit | null>}
     */
    async renameFor(relativePath, line, character, newName) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      return sendRequest(RenameRequest.type, {
        textDocument: { uri },
        position: { line, character },
        newName,
      });
    },

    /**
     * Send a signatureHelp request for a position in a file.
     * @param {string} relativePath - Path relative to the sandbox root
     * @param {number} line - Zero-based line number
     * @param {number} character - Zero-based character offset
     * @returns {Promise<import("vscode-languageserver-protocol").SignatureHelp | null>}
     */
    async signatureHelpFor(relativePath, line, character) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      return sendRequest(SignatureHelpRequest.type, {
        textDocument: { uri },
        position: { line, character },
      });
    },

    /**
     * Notify the LSP server that a file was opened.
     * @param {string} relativePath - Path relative to the sandbox root
     */
    openFile(relativePath) {
      const uri = toUri(relativePath);
      const text = readFileSync(
        path.join(realpathSync(cwd), relativePath),
        "utf8",
      );
      openedFiles.add(uri);
      connection.sendNotification(DidOpenTextDocumentNotification.type, {
        textDocument: {
          uri,
          languageId: "rescript",
          version: nextVersion++,
          text,
        },
      });
    },

    /**
     * Notify the LSP server that a file was closed.
     * @param {string} relativePath - Path relative to the sandbox root
     */
    closeFile(relativePath) {
      const uri = toUri(relativePath);
      openedFiles.delete(uri);
      connection.sendNotification(DidCloseTextDocumentNotification.type, {
        textDocument: { uri },
      });
    },

    /**
     * Send a formatting request for a file.
     * @param {string} relativePath - Path relative to the sandbox root
     * @returns {Promise<import("vscode-languageserver-protocol").TextEdit[]>}
     */
    async formatFor(relativePath) {
      const uri = toUri(relativePath);
      assertOpen(relativePath, uri);
      const result = await sendRequest(DocumentFormattingRequest.type, {
        textDocument: { uri },
        options: { tabSize: 2, insertSpaces: true },
      });
      return result || [];
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
     * Get all diagnostics collected so far (non-blocking).
     * Returns a sorted array of { file, diagnostics } with sandbox-relative paths.
     * @returns {Array<{file: string, diagnostics: Array<{range: any, severity: number, message: string}>}>}
     */
    getDiagnostics() {
      return [...diagnosticsByPath.entries()]
        .map(([file, diagnostics]) => ({
          file,
          diagnostics: diagnostics.map(d => ({
            range: d.range,
            severity: d.severity,
            message: d.message,
          })),
        }))
        .sort((a, b) => a.file.localeCompare(b.file));
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
