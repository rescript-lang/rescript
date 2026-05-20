#!/usr/bin/env node

import * as fs from "node:fs/promises";
import * as path from "node:path";

const devPlaygroundDir = path.join(import.meta.dirname, "..");
const repoRoot = path.join(devPlaygroundDir, "..", "..");
const playgroundDir = path.join(repoRoot, "packages", "playground");
const sourceCompiler = path.join(playgroundDir, "compiler.js");
const sourcePackages = path.join(playgroundDir, "packages");
const targetRoot = path.join(
  devPlaygroundDir,
  "public",
  "playground-bundles",
  "local",
);

async function assertExists(filePath, message) {
  try {
    await fs.stat(filePath);
  } catch {
    throw new Error(`${message}: ${filePath}`);
  }
}

await assertExists(
  sourceCompiler,
  "Missing playground compiler bundle. Run `make playground` first",
);
await assertExists(
  sourcePackages,
  "Missing playground cmij packages. Run `make playground` first",
);

await fs.rm(targetRoot, {recursive: true, force: true});
await fs.mkdir(targetRoot, {recursive: true});
await fs.copyFile(sourceCompiler, path.join(targetRoot, "compiler.js"));
await fs.cp(sourcePackages, targetRoot, {recursive: true});

console.log(`Staged local playground bundle at ${targetRoot}`);
