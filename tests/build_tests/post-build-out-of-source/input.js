// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const isWindows = process.platform === "win32";

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

  // With in-source: false and module: esmodule, the paths should be in lib/es6/
  // e.g., /path/to/post-build-out-of-source/lib/es6/src/demo.mjs (Unix)
  // e.g., C:\path\to\post-build-out-of-source\lib\es6\src\demo.mjs (Windows)
  const libEs6Sep = isWindows ? "\\lib\\es6\\" : "/lib/es6/";
  const libBsSep = isWindows ? "\\lib\\bs\\" : "/lib/bs/";

  for (const p of paths) {
    assert.ok(
      p.includes(libEs6Sep) && p.endsWith(".mjs"),
      `Path should be in lib/es6/ directory with .mjs suffix: ${p}`,
    );
    // Should NOT be in lib/bs/ when in-source is false
    assert.ok(!p.includes(libBsSep), `Path should not be in lib/bs/: ${p}`);
  }
} finally {
  // Clean up log file
  if (fs.existsSync(logFile)) {
    fs.unlinkSync(logFile);
  }
  await execClean();
}
