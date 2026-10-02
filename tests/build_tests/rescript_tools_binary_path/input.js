// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { rescript_tools_exe } from "#cli/bins";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean, node } = setup(import.meta.dirname);

const repoRoot = path.resolve(import.meta.dirname, "..", "..", "..");
const nodeModules = path.join(import.meta.dirname, "node_modules");

await execBuildOrThrow();

// The project depends on the rescript package, as a user project does.
await fs.mkdir(nodeModules, { recursive: true });
await fs.symlink(repoRoot, path.join(nodeModules, "rescript"), "junction");

try {
  // The project emits both ES module and CommonJS output.
  for (const output of ["Main.mjs", "Main.cjs"]) {
    const out = await node(path.join("src", output));
    assert.equal(out.stderr, "", output);
    assert.equal(out.stdout.trim(), rescript_tools_exe, output);
  }
} finally {
  await fs.rm(nodeModules, { recursive: true, force: true });
  await execClean();
}
