/**
 * Test context for rewatch tests.
 *
 * Provides a unified test harness that:
 * 1. Creates an isolated sandbox copy of the fixture
 * 2. Starts an OTEL receiver to collect traces
 * 3. Runs the test scenario with CLI helpers
 * 4. Builds a span summary for snapshot testing
 * 5. Cleans up resources
 */

import { existsSync } from "node:fs";
import { mkdir, readFile, rename, unlink, writeFile } from "node:fs/promises";
import path from "node:path";
import { expect } from "vitest";
import { createOtelReceiver } from "./otel-receiver.mjs";
import { createRescriptCli } from "./process.mjs";
import { createSandbox, removeSandbox } from "./sandbox.mjs";

/**
 * @typedef {object} SpanNode
 * @property {string} name - Span name
 * @property {object} [attrs] - Selected attributes (if any)
 * @property {SpanNode[]} [children] - Child spans (if any)
 */

/**
 * @typedef {object} TestContext
 * @property {string} sandbox - Path to the test sandbox
 * @property {ReturnType<typeof createRescriptCli>} cli - CLI helper scoped to sandbox root
 * @property {(relativePath: string) => ReturnType<typeof createRescriptCli>} createCli - Create a CLI helper scoped to a subdirectory of the sandbox
 * @property {(relativePath: string, content: string) => Promise<void>} writeFileInSandbox - Write a file in the sandbox (relative path resolved against sandbox root, uses atomic writes)
 * @property {(relativePath: string) => Promise<string>} readFileInSandbox - Read a file from the sandbox (relative path resolved against sandbox root)
 * @property {(filePath: string) => Promise<void>} deleteFile - Delete a file
 * @property {(filePath: string) => boolean} fileExists - Check if file exists
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
 * Span names to include in the summary.
 * Other spans are filtered out to reduce noise.
 */
const SUMMARY_SPAN_NAMES = new Set([
  // Top-level command spans
  "rewatch.build",
  "rewatch.clean",
  "rewatch.watch",
  "rewatch.format",
  "rewatch.compiler_args",
  // Build pipeline spans
  "initialize_build",
  "incremental_build",
  "packages.make",
  "packages.parse_packages",
  "build.load_package_sources",
  "build.parse",
  "build.parse_file",
  "build.parse_error",
  "build.compile",
  "build.compile_wave",
  "build.compile_file",
  "build.compile_error",
  "build.compile_warning",
  "build.js_post_build",
  // Clean spans
  "clean.clean",
  "clean.cleanup_previous_build",
  // Format spans
  "format.format",
  "format.write_file",
]);

/**
 * Attributes to include in the summary for specific span types.
 */
const SUMMARY_ATTRS = {
  "rewatch.build": ["working_dir"],
  "rewatch.clean": ["working_dir"],
  "rewatch.watch": ["working_dir"],
  "rewatch.format": ["check", "is_stdin"],
  "rewatch.compiler_args": ["file_path"],
  incremental_build: ["module_count"],
  "build.load_package_sources": ["package"],
  "build.parse": ["dirty_modules"],
  "build.parse_file": [
    "module",
    "package",
    "ppx",
    "experimental",
    "jsx",
    "bsc_flags",
  ],
  "build.compile_wave": ["file_count"],
  "build.compile_file": [
    "module",
    "package",
    "suffix",
    "module_system",
    "namespace",
  ],
  "build.js_post_build": ["command", "js_file"],
  "format.write_file": ["file"],
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
  "build.parse_file",
  "build.compile_file",
  "format.write_file",
];

/**
 * Sort consecutive blocks matching parallel span patterns alphabetically.
 * A "block" is a matching line plus any deeper-indented children below it.
 * This ensures spans with children (e.g., compile_file with js_post_build)
 * are sorted as a unit rather than being split by their children.
 *
 * @param {string[]} lines - Summary lines
 * @returns {string[]} - Lines with parallel span blocks sorted
 */
function sortParallelSpans(lines) {
  const result = [];
  let currentPattern = null;
  // Each block is { key: string, lines: string[] }
  let collectedBlocks = [];
  let currentBlockIndent = 0;

  function flushBlocks() {
    if (collectedBlocks.length > 0) {
      collectedBlocks.sort((a, b) => a.key.localeCompare(b.key));
      for (const block of collectedBlocks) {
        result.push(...block.lines);
      }
      collectedBlocks = [];
    }
  }

  for (const line of lines) {
    const matchedPattern = PARALLEL_SPAN_PATTERNS.find(p => line.includes(p));
    const indent = line.search(/\S/);

    // If we're collecting blocks and this line is a deeper-indented child,
    // append it to the current block
    if (
      collectedBlocks.length > 0 &&
      !matchedPattern &&
      indent > currentBlockIndent
    ) {
      collectedBlocks[collectedBlocks.length - 1].lines.push(line);
      continue;
    }

    if (matchedPattern && matchedPattern === currentPattern) {
      // Another block of the same pattern
      collectedBlocks.push({ key: line.trim(), lines: [line] });
      currentBlockIndent = indent;
    } else {
      // Different pattern or not a pattern — flush and reset
      flushBlocks();

      if (matchedPattern) {
        currentPattern = matchedPattern;
        currentBlockIndent = indent;
        collectedBlocks.push({ key: line.trim(), lines: [line] });
      } else {
        currentPattern = null;
        result.push(line);
      }
    }
  }

  flushBlocks();

  return result;
}

/**
 * Normalize absolute paths in span attributes to sandbox-relative paths.
 *
 * @param {string[]} summary - Summary lines
 * @param {string} sandboxPath - The sandbox root path
 * @returns {string[]} - Summary with normalized paths
 */
export function normalizePaths(summary, sandboxPath) {
  // Match individual attr=value pairs
  const attrValueRegex = /(\w+)=([^,\]]+)/g;

  return summary.map(line => {
    return line.replace(attrValueRegex, (match, attrName, value) => {
      // Check if this looks like an absolute path
      if (value.startsWith("/") || value.match(/^[A-Z]:\\/)) {
        let relativePath = path
          .relative(sandboxPath, value)
          .split(path.sep)
          .join("/");
        // Use "." for the sandbox root itself
        if (relativePath === "") {
          relativePath = ".";
        } else if (relativePath.startsWith("..")) {
          // Path is outside sandbox, keep original
          return match;
        }
        return `${attrName}=${relativePath}`;
      }
      return match;
    });
  });
}

/**
 * Run a rewatch test with snapshot-based span assertions.
 *
 * The test flow is:
 * 1. Setup: Create sandbox, start OTEL receiver
 * 2. Scenario: Execute your test operations (build, clean, watch, etc.)
 * 3. Assert: Snapshot the span tree summary
 * 4. Cleanup: Remove sandbox, stop OTEL receiver
 *
 * @param {(ctx: TestContext) => Promise<void>} scenario - The test scenario to execute
 * @param {object} [options] - Options for the test
 * @param {(summary: string[], sandboxPath: string) => string[]} [options.processSpans] - Transform the span summary before snapshot
 * @returns {Promise<void>}
 */
export async function runRewatchTest(scenario, options = {}) {
  let otelReceiver = null;
  let sandbox = null;
  const watchHandles = [];

  try {
    // Setup
    otelReceiver = await createOtelReceiver();
    sandbox = await createSandbox();

    // Create a CLI helper that tracks watch handles for automatic cleanup
    function createTrackedCli(cwd) {
      const inner = createRescriptCli(cwd, otelReceiver.endpoint);
      return {
        ...inner,
        spawnWatch(args) {
          const handle = inner.spawnWatch(args);
          watchHandles.push(handle);
          return handle;
        },
      };
    }

    const cli = createTrackedCli(sandbox);

    // Create test context
    const ctx = {
      sandbox,
      cli,
      createCli(relativePath) {
        return createTrackedCli(path.join(sandbox, relativePath));
      },

      async writeFileInSandbox(relativePath, content) {
        const fullPath = path.join(sandbox, relativePath);
        // Ensure parent directory exists
        await mkdir(path.dirname(fullPath), { recursive: true });
        // Use atomic write (temp file + rename) to prevent the watcher from
        // seeing a truncated file. On Linux, fs.writeFile generates two
        // inotify IN_MODIFY events (truncate + write) which can cause the
        // watcher to read the file while it's empty.
        const tmpPath = fullPath + ".__atomic_tmp";
        await writeFile(tmpPath, content);
        await rename(tmpPath, fullPath);
      },

      async readFileInSandbox(relativePath) {
        const fullPath = path.join(sandbox, relativePath);
        return readFile(fullPath, "utf8");
      },

      async deleteFile(filePath) {
        const fullPath = path.isAbsolute(filePath)
          ? filePath
          : path.join(sandbox, filePath);
        await unlink(fullPath);
      },

      fileExists(filePath) {
        const fullPath = path.isAbsolute(filePath)
          ? filePath
          : path.join(sandbox, filePath);
        return existsSync(fullPath);
      },
    };

    // Execute scenario
    await scenario(ctx);
  } finally {
    // Stop any watch processes to flush telemetry
    for (const handle of watchHandles) {
      try {
        await handle.stop();
      } catch (err) {
        console.error("[runRewatchTest] Error stopping watch process:", err);
      }
    }
  }

  try {
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
      console.log(`[runRewatchTest] Span summary:`);
      for (const line of summary) {
        console.log(`  ${line}`);
      }
    }

    // Snapshot assertion - wrap in try/catch to log debug info on failure
    try {
      expect(summary).toMatchSnapshot();
    } catch (err) {
      // Log raw telemetry data to help debug CI failures
      console.error("\n=== SNAPSHOT ASSERTION FAILED ===");
      console.error("Platform:", process.platform);
      console.error("Sandbox path:", sandbox);
      // Dump rewatch -vv stderr from watch processes
      for (let i = 0; i < watchHandles.length; i++) {
        const stderr = watchHandles[i].getStderr?.();
        if (stderr) {
          console.error(`\n--- Watch process ${i} stderr (-vv) ---`);
          console.error(stderr);
        }
      }
      console.error("\n--- Raw spans (%d total) ---", spans.length);
      for (const span of spans) {
        console.error(JSON.stringify(span, null, 2));
      }
      console.error("\n--- Span tree ---");
      console.error(JSON.stringify(tree, null, 2));
      console.error("\n--- Summary (actual) ---");
      for (const line of summary) {
        console.error(`  ${JSON.stringify(line)}`);
      }
      console.error("=== END DEBUG INFO ===\n");
      throw err;
    }
  } finally {
    if (sandbox) {
      await removeSandbox(sandbox);
    }
    if (otelReceiver) {
      await otelReceiver.stop();
    }
  }
}
