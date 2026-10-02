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
  const out = await node(path.join("src", "Main.js"));
  assert.equal(out.stderr, "");
  assert.equal(out.stdout.trim(), rescript_tools_exe);
} finally {
  await fs.rm(nodeModules, { recursive: true, force: true });
  await execClean();
}
