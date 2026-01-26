/**
 * Debug script to visualize the span hierarchy from a build.
 *
 * Usage: bun run debug-spans.mjs
 *
 * This starts an OTLP receiver, runs a build, and prints the span tree.
 */

import { execSync } from "node:child_process";
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

  console.log(`Starting build with OTEL endpoint: ${otel.endpoint}`);

  // Run a build with otel enabled
  try {
    execSync(`${rescriptBin} build`, {
      cwd: fixtureDir,
      env: {
        ...process.env,
        OTEL_EXPORTER_OTLP_ENDPOINT: otel.endpoint,
        RESCRIPT_BSC_EXE: bscExe,
        RESCRIPT_RUNTIME: runtime,
      },
      stdio: "inherit",
    });
  } catch {
    // Build might fail, that's ok for this test
  }

  // Wait a bit for spans to arrive
  await new Promise(r => setTimeout(r, 1000));

  const spans = otel.getSpans();
  console.log("\n=== SPAN HIERARCHY ===\n");

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
    console.log(`${prefix}${span.name} (${duration})`);

    // Print selected attributes
    if (span.attributes) {
      const interesting = [
        "client_id",
        "working_dir",
        "package",
        "dirty_modules",
        "module_count",
        "scoped",
      ];
      for (const key of interesting) {
        if (span.attributes[key] !== undefined && span.attributes[key] !== "") {
          console.log(
            `${prefix}  [${key}]: ${JSON.stringify(span.attributes[key])}`,
          );
        }
      }
    }

    // Find children
    const children = spans.filter(s => s.parentSpanId === span.spanId);
    for (const child of children) {
      printSpan(child, indent + 1);
    }
  }

  console.log(`Found ${roots.length} root span(s):\n`);
  for (const root of roots) {
    printSpan(root);
    console.log("");
  }

  otel.stop();
  process.exit(0);
}

main().catch(console.error);
