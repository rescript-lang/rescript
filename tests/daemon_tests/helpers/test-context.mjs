/**
 * Test context v2: Execute scenario, shutdown daemon, then assert on spans.
 *
 * Uses snapshot testing to verify the span tree structure.
 * Spans are converted to a nested tree and serialized for comparison.
 */

import { existsSync, readFileSync } from "node:fs";
import { unlink, writeFile } from "node:fs/promises";
import path, { join } from "node:path";
import { expect } from "vitest";
import { startDaemon, stopDaemon } from "./daemon.mjs";
import {
  collectStream,
  createClient,
  createDebugClient,
} from "./grpc-client.mjs";
import { createOtelReceiver } from "./otel-receiver.mjs";
import { createRescriptCli } from "./process.mjs";
import { createSandbox, removeSandbox } from "./sandbox.mjs";

/**
 * @typedef {object} WatchHandle
 * @property {() => Promise<void>} waitForBuild - Wait for a build to complete
 * @property {() => void} stop - Stop the watch process
 */

/**
 * @typedef {object} ScenarioContext
 * @property {string} sandbox - Path to the test sandbox
 * @property {(workingDir: string) => Promise<void>} build - Execute a build
 * @property {(workingDir: string) => Promise<void>} clean - Execute a clean
 * @property {(workingDir: string) => Promise<WatchHandle>} watch - Start watch mode
 * @property {(filePath: string, content: string) => Promise<void>} writeFile - Write a file
 * @property {(filePath: string) => Promise<void>} deleteFile - Delete a file
 * @property {(filePath: string) => boolean} fileExists - Check if file exists
 */

/**
 * @typedef {object} SpanNode
 * @property {string} name - Span name
 * @property {object} [attrs] - Selected attributes (if any)
 * @property {SpanNode[]} [children] - Child spans (if any)
 */

/**
 * Convert flat spans to a tree structure based on parent-child relationships.
 * @param {Array<object>} spans - Flat array of spans
 * @returns {SpanNode[]} - Tree of span nodes
 */
function buildSpanTree(spans) {
  // Create a map of spanId -> span
  const spanMap = new Map();
  for (const span of spans) {
    spanMap.set(span.spanId, span);
  }

  // Group spans by parentSpanId
  const childrenMap = new Map();
  const roots = [];

  for (const span of spans) {
    if (!span.parentSpanId || !spanMap.has(span.parentSpanId)) {
      roots.push(span);
    } else {
      const parentId = span.parentSpanId;
      if (!childrenMap.has(parentId)) {
        childrenMap.set(parentId, []);
      }
      childrenMap.get(parentId).push(span);
    }
  }

  // Sort children by start time
  for (const children of childrenMap.values()) {
    children.sort((a, b) => {
      const aStart = BigInt(a.startTimeUnixNano || 0);
      const bStart = BigInt(b.startTimeUnixNano || 0);
      if (aStart < bStart) return -1;
      if (aStart > bStart) return 1;
      return 0;
    });
  }

  // Sort roots by start time
  roots.sort((a, b) => {
    const aStart = BigInt(a.startTimeUnixNano || 0);
    const bStart = BigInt(b.startTimeUnixNano || 0);
    if (aStart < bStart) return -1;
    if (aStart > bStart) return 1;
    return 0;
  });

  // Recursively build tree
  function buildNode(span) {
    const children = childrenMap.get(span.spanId) || [];
    const node = { name: span.name };

    // Include selected attributes
    if (span.attributes && Object.keys(span.attributes).length > 0) {
      node.attrs = span.attributes;
    }

    if (children.length > 0) {
      node.children = children.map(buildNode);
    }

    return node;
  }

  return roots.map(buildNode);
}

/**
 * Span names to include in the summary (top-level operations).
 * Other spans are filtered out to reduce noise.
 */
const SUMMARY_SPAN_NAMES = new Set([
  "rpc.build",
  "rpc.clean",
  "rpc.watch",
  "rpc.format",
  "rpc.get_compiler_args",
  "rpc.get_clients",
  "clean.execute",
  "work_queue.handle_build",
  "work_queue.handle_file_change_build",
  "work_queue.watch_initial_build",
  "work_queue.handle_format_collect_files",
  "build.incremental_build",
  "build.load_package_sources",
  "build.parse",
  "build.parse_error",
  "build.compile",
  "build.compile_error",
  "build.compile_warning",
  "build.initialize_packages",
  "build.js_post_build",
  "build.js_post_build_stdout",
  "build.js_post_build_stderr",
  "build.js_post_build_error",
  "file_change",
  "format.execute",
  "format.stdin",
  "format.write_file",
]);

/**
 * Attributes to include in the summary for specific span types.
 */
const SUMMARY_ATTRS = {
  "rpc.build": ["working_dir", "filter", "warn_error"],
  "rpc.clean": ["working_dir"],
  "rpc.watch": ["working_dir"],
  "rpc.format": ["working_dir", "check", "is_stdin"],
  "rpc.get_clients": ["client_count"],
  "clean.execute": ["scope"],
  "build.incremental_build": ["module_count"],
  "build.load_package_sources": ["package"],
  "build.parse": ["dirty_modules"],
  "build.js_post_build": ["command", "js_file"],
  file_change: ["path", "change_type"],
  "format.execute": ["file_count", "check"],
  "format.stdin": ["ext"],
  "format.write_file": ["file"],
  "rpc.get_compiler_args": ["file_path"],
};

/**
 * Convert a span tree to a simplified summary for snapshots.
 * @param {SpanNode[]} tree - The span tree
 * @returns {Array} - Simplified summary
 */
function treeToSummary(tree) {
  const result = [];

  function processNode(node, depth = 0) {
    // Only include spans we care about
    if (!SUMMARY_SPAN_NAMES.has(node.name)) {
      // Still process children even if we skip this node
      if (node.children) {
        for (const child of node.children) {
          processNode(child, depth);
        }
      }
      return;
    }

    // Build summary entry
    let entry = node.name;

    // Add selected attributes (skip empty values)
    const attrsToInclude = SUMMARY_ATTRS[node.name];
    if (attrsToInclude && node.attrs) {
      const parts = [];
      for (const attr of attrsToInclude) {
        const value = node.attrs[attr];
        if (value !== undefined && value !== "") {
          parts.push(`${attr}=${value}`);
        }
      }
      if (parts.length > 0) {
        entry += `[${parts.join(", ")}]`;
      }
    }

    // Add indentation for nesting
    const indent = "  ".repeat(depth);
    result.push(indent + entry);

    // Process children
    if (node.children) {
      for (const child of node.children) {
        processNode(child, depth + 1);
      }
    }
  }

  for (const root of tree) {
    processNode(root);
  }

  // Sort consecutive parallel spans alphabetically for deterministic snapshots
  return sortParallelSpans(result);
}

/**
 * Span patterns that run in parallel and need sorting for deterministic output.
 */
const PARALLEL_SPAN_PATTERNS = [
  "build.load_package_sources",
  "format.write_file",
];

/**
 * Sort consecutive lines matching parallel span patterns alphabetically.
 * @param {string[]} lines - Summary lines
 * @returns {string[]} - Lines with parallel spans sorted
 */
function sortParallelSpans(lines) {
  const result = [];
  let currentPattern = null;
  let collectedLines = [];

  for (const line of lines) {
    const matchedPattern = PARALLEL_SPAN_PATTERNS.find(p => line.includes(p));

    if (matchedPattern && matchedPattern === currentPattern) {
      // Continue collecting lines of the same pattern
      collectedLines.push(line);
    } else {
      // Flush any collected lines (sorted)
      if (collectedLines.length > 0) {
        collectedLines.sort();
        result.push(...collectedLines);
        collectedLines = [];
      }

      if (matchedPattern) {
        // Start collecting a new pattern
        currentPattern = matchedPattern;
        collectedLines.push(line);
      } else {
        // Not a parallel span, just add it
        currentPattern = null;
        result.push(line);
      }
    }
  }

  // Flush remaining collected lines
  if (collectedLines.length > 0) {
    collectedLines.sort();
    result.push(...collectedLines);
  }

  return result;
}

/**
 * Normalize absolute paths in span attributes to sandbox-relative paths.
 * Use as a processSpans callback.
 *
 * @param {string[]} summary - Summary lines
 * @param {string} sandboxPath - The sandbox root path
 * @returns {string[]} - Summary with normalized paths
 */
export function normalizePaths(summary, sandboxPath) {
  // Match individual attr=value pairs, stopping at comma or closing bracket
  // This handles multiple attributes like: spanName[attr1=val1, attr2=val2]
  const attrValueRegex = /(\w+)=([^,\]]+)/g;

  return summary.map(line => {
    return line.replace(attrValueRegex, (match, attrName, value) => {
      // Check if this looks like an absolute path
      if (value.startsWith("/") || value.match(/^[A-Z]:\\/)) {
        let relativePath = path
          .relative(sandboxPath, value)
          .split(path.sep)
          .join("/");
        // Use "." for the sandbox root itself, skip paths outside sandbox
        if (relativePath === "") {
          relativePath = ".";
        } else if (relativePath.startsWith("..")) {
          // Path is outside sandbox, keep original (shouldn't happen normally)
          return match;
        }
        return `${attrName}=${relativePath}`;
      }
      return match;
    });
  });
}

/**
 * Read a file's contents, returning empty string if it doesn't exist.
 * @param {string} filePath
 * @returns {string}
 */
function readFileSafe(filePath) {
  try {
    return readFileSync(filePath, "utf-8");
  } catch {
    return "";
  }
}

/**
 * Dump diagnostic information to stderr when a test fails or times out.
 * @param {string|null} sandbox - Sandbox path (for daemon log files)
 * @param {object|null} daemon - Daemon process info
 * @param {object|null} otelReceiver - OTEL receiver (for collected spans)
 */
function dumpDiagnostics(sandbox, daemon, otelReceiver) {
  const now = new Date().toISOString();
  console.error(`\n=== DAEMON TEST DIAGNOSTICS (${now}) ===\n`);

  // Daemon process state
  if (daemon) {
    const pid = daemon.process.pid;
    let alive = false;
    try {
      process.kill(pid, 0);
      alive = true;
    } catch {}
    console.error(`Daemon PID: ${pid}, alive: ${alive}`);
  } else {
    console.error("Daemon: not started");
  }

  // Daemon logs
  if (sandbox) {
    const logContent = readFileSafe(join(sandbox, "lib", "bs", "daemon.log"));
    const errContent = readFileSafe(join(sandbox, "lib", "bs", "daemon.err"));

    if (logContent) {
      console.error("\n--- daemon.log ---");
      console.error(logContent);
    } else {
      console.error("\n--- daemon.log: (empty) ---");
    }

    if (errContent) {
      console.error("\n--- daemon.err ---");
      console.error(errContent);
    }
  }

  // Collected OTEL spans
  if (otelReceiver) {
    const spans = otelReceiver.getSpans();
    console.error(`\n--- OTEL spans collected: ${spans.length} ---`);
    if (spans.length > 0) {
      for (const span of spans) {
        const startMs = span.startTimeUnixNano
          ? new Date(
              Number(BigInt(span.startTimeUnixNano) / 1_000_000n),
            ).toISOString()
          : "?";
        const duration =
          span.endTimeUnixNano && span.startTimeUnixNano
            ? Number(
                (BigInt(span.endTimeUnixNano) -
                  BigInt(span.startTimeUnixNano)) /
                  1_000_000n,
              )
            : "?";
        const status = span.status?.code ? ` status=${span.status.code}` : "";
        console.error(`  [${startMs}] ${span.name} (${duration}ms)${status}`);
      }
    }
  }

  console.error("\n=== END DIAGNOSTICS ===\n");
}

/**
 * Run a daemon test with snapshot-based span assertions.
 *
 * The test flow is:
 * 1. Setup: Create sandbox, start daemon, start otel receiver
 * 2. Scenario: Execute your test operations (build, clean, watch, etc.)
 * 3. Shutdown: Daemon is stopped, spans are flushed
 * 4. Assert: Snapshot the span tree summary
 * 5. Cleanup: Remove sandbox, stop otel receiver
 *
 * @param {(ctx: ScenarioContext) => Promise<void>} scenario - The test scenario to execute
 * @param {object} [options] - Options for the test
 * @param {(summary: string[]) => string[]} [options.processSpans] - Transform the span summary before snapshot
 * @returns {Promise<void>}
 */
export async function runDaemonTest(scenario, options = {}) {
  let otelReceiver = null;
  let daemon = null;
  let sandbox = null;
  let debugClient = null;
  const watchHandles = [];

  // Use onTestFailed to dump diagnostics when vitest aborts us (e.g. timeout)
  const { onTestFailed } = await import("vitest");
  onTestFailed(() => {
    dumpDiagnostics(sandbox, daemon, otelReceiver);
  });

  try {
    // Setup
    otelReceiver = await createOtelReceiver();
    sandbox = await createSandbox();
    daemon = await startDaemon(sandbox, {
      otelEndpoint: otelReceiver.endpoint,
    });

    // Keep a debug client connected to prevent daemon from shutting down
    debugClient = await createDebugClient(daemon.socketPath);

    const client = createClient(daemon.socketPath);

    // Create scenario context
    const ctx = {
      sandbox,
      grpcClient: client,
      daemon,
      debug: debugClient,

      async build(workingDirectory) {
        await collectStream(
          client.Build({ working_directory: workingDirectory }),
        );
      },

      async clean(workingDirectory) {
        await collectStream(
          client.Clean({ working_directory: workingDirectory }),
        );
      },

      async watch(workingDirectory) {
        // Create a CLI scoped to the working directory
        const watchCli = createRescriptCli(workingDirectory);
        const watchProc = watchCli.spawnWatch();
        let buildCount = 0;
        let buildResolvers = [];

        // Listen to debug events for build_finished
        const onData = event => {
          if (event.build_finished != null) {
            buildCount++;
            // Resolve any waiting promises
            const toResolve = buildResolvers;
            buildResolvers = [];
            for (const resolve of toResolve) {
              resolve(event.build_finished);
            }
          }
        };
        debugClient.stream.on("data", onData);

        const handle = {
          async waitForBuild() {
            return new Promise(resolve => {
              buildResolvers.push(resolve);
            });
          },

          async waitForSettle(ms = 500) {
            // Wait for builds to stop arriving
            let lastCount = buildCount;
            while (true) {
              await new Promise(r => setTimeout(r, ms));
              if (buildCount === lastCount) break;
              lastCount = buildCount;
            }
          },

          stop() {
            debugClient.stream.off("data", onData);
            watchProc.stop();
          },
        };

        watchHandles.push(handle);

        // Wait for initial build to complete
        await handle.waitForBuild();
        // Wait for the cascade to settle (mjs files trigger rebuilds)
        await handle.waitForSettle();

        return handle;
      },

      async writeFile(filePath, content) {
        await writeFile(filePath, content);
      },

      async deleteFile(filePath) {
        await unlink(filePath);
      },

      fileExists(filePath) {
        return existsSync(filePath);
      },
    };

    // Execute scenario
    await scenario(ctx);

    // Stop any watch handles that weren't stopped
    for (const handle of watchHandles) {
      try {
        handle.stop();
      } catch {}
    }

    // Shutdown daemon to flush spans
    if (debugClient) {
      debugClient.close();
      debugClient = null;
    }
    await stopDaemon(daemon);
    daemon = null;

    // Wait for spans to arrive
    await new Promise(r => setTimeout(r, 500));

    // Get spans and build tree
    const spans = otelReceiver.getSpans();
    const tree = buildSpanTree(spans);
    let summary = treeToSummary(tree);

    // Always normalize absolute paths to sandbox-relative paths
    summary = normalizePaths(summary, sandbox);

    // Apply any additional processing callback
    if (options.processSpans) {
      summary = options.processSpans(summary, sandbox);
    }

    if (process.env.DEBUG_OTEL) {
      console.log(`[runDaemonTest] Span summary:`);
      for (const line of summary) {
        console.log(`  ${line}`);
      }
    }

    // Snapshot assertion
    expect(summary).toMatchSnapshot();
  } finally {
    // Cleanup watch handles
    for (const handle of watchHandles) {
      try {
        handle.stop();
      } catch {}
    }
    // Cleanup
    if (debugClient) {
      try {
        debugClient.close();
      } catch {}
    }
    if (daemon) {
      await stopDaemon(daemon);
    }
    if (sandbox) {
      await removeSandbox(sandbox);
    }
    if (otelReceiver) {
      await otelReceiver.stop();
    }
  }
}
