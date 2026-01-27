/**
 * Debug script to visualize the span hierarchy from watch mode.
 */

import { spawn } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { createOtelReceiver } from "./helpers/otel-receiver.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const fixtureDir = path.join(__dirname, "fixture");
const projectRoot = path.join(__dirname, "../..");
const rescriptBin = path.join(
  projectRoot,
  "packages/@rescript/darwin-arm64/bin/rescript.exe",
);
const bscExe = path.join(
  projectRoot,
  "_build/default/compiler/bsc/rescript_compiler_main.exe",
);
const runtime = path.join(projectRoot, "packages/@rescript/runtime");

async function main() {
  const otel = await createOtelReceiver();

  console.log(`Starting watch with OTEL endpoint: ${otel.endpoint}`);

  // Run watch mode
  const watchProc = spawn(rescriptBin, ["watch"], {
    cwd: fixtureDir,
    env: {
      ...process.env,
      OTEL_EXPORTER_OTLP_ENDPOINT: otel.endpoint,
      RESCRIPT_BSC_EXE: bscExe,
      RESCRIPT_RUNTIME: runtime,
    },
    stdio: "inherit",
  });

  // Wait for initial build to complete
  console.log("Waiting 3 seconds for initial build...");
  await new Promise(r => setTimeout(r, 3000));

  // Kill the watch process with SIGINT (Ctrl-C) for cleaner shutdown
  console.log("Sending SIGINT to watch process...");
  watchProc.kill("SIGINT");

  // Wait for process to exit
  await new Promise(resolve => {
    watchProc.on("exit", (code, signal) => {
      console.log(`Watch process exited with code=${code}, signal=${signal}`);
      resolve();
    });
  });

  // Wait for spans to be flushed
  console.log("Waiting 3 seconds for spans to flush...");
  await new Promise(r => setTimeout(r, 3000));

  const spans = otel.getSpans();
  console.log("\n=== SPAN HIERARCHY ===\n");
  console.log(`Total spans received: ${spans.length}`);

  // Build a map of span ID to span
  const spanMap = new Map();
  for (const span of spans) {
    spanMap.set(span.spanId, span);
  }

  // Find root spans (no parent or parent not in our spans)
  const roots = spans.filter(
    s => !s.parentSpanId || !spanMap.has(s.parentSpanId),
  );

  function printSpan(span, indent = 0) {
    const prefix = "  ".repeat(indent);
    const duration =
      span.endTimeUnixNano && span.startTimeUnixNano
        ? `${((Number(span.endTimeUnixNano) - Number(span.startTimeUnixNano)) / 1_000_000).toFixed(2)}ms`
        : "?";
    console.log(
      `${prefix}${span.name} [traceId=${span.traceId?.slice(0, 8)}, spanId=${span.spanId?.slice(0, 8)}, parent=${span.parentSpanId?.slice(0, 8) || "none"}] (${duration})`,
    );

    // Find children
    const children = spans.filter(s => s.parentSpanId === span.spanId);
    for (const child of children) {
      printSpan(child, indent + 1);
    }
  }

  console.log(`\nFound ${roots.length} root span(s):\n`);
  for (const root of roots) {
    printSpan(root);
    console.log("");
  }

  // Print all span names for quick overview
  console.log("=== ALL SPAN NAMES ===");
  for (const span of spans) {
    console.log(
      `  ${span.name} [spanId=${span.spanId?.slice(0, 8)}, parent=${span.parentSpanId?.slice(0, 8) || "none"}]`,
    );
  }

  otel.stop();
  process.exit(0);
}

main().catch(console.error);
