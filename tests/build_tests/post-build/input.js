// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const isWindows = process.platform === "win32";

// Use a node script for cross-platform path logging
const logFile = path.join(import.meta.dirname, "post-build-paths.txt");

// Clean up any previous log file
if (fs.existsSync(logFile)) {
  fs.unlinkSync(logFile);
}

const out = await execBuild();

if (out.status !== 0) {
  assert.fail(out.stdout + out.stderr);
}

try {
  // Verify that the post-build command received the correct paths
  assert.ok(fs.existsSync(logFile), "post-build-paths.txt should exist");

  const paths = fs.readFileSync(logFile, "utf-8").trim().split("\n");

  // With in-source: true, the paths should be next to the source files
  // e.g., /path/to/post-build/src/demo.js (Unix)
  // e.g., C:\path\to\post-build\src\demo.js (Windows)
  const srcSep = isWindows ? "\\src\\" : "/src/";
  const libBsSep = isWindows ? "\\lib\\bs\\" : "/lib/bs/";

  for (const p of paths) {
    assert.ok(
      p.includes(srcSep) && p.endsWith(".js"),
      `Path should be in src/ directory: ${p}`,
    );
    // Should NOT be in lib/bs/ when in-source is true
    assert.ok(
      !p.includes(libBsSep),
      `Path should not be in lib/bs/ when in-source is true: ${p}`,
    );
  }
} finally {
  // Clean up log file
  if (fs.existsSync(logFile)) {
    fs.unlinkSync(logFile);
  }
  await execClean();
}
