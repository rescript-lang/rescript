#!/usr/bin/env node

import * as fs from "node:fs/promises";
import * as path from "node:path";

const devPlaygroundDir = path.join(import.meta.dirname, "..");
const repoRoot = path.join(devPlaygroundDir, "..", "..");
const playgroundDir = path.join(repoRoot, "packages", "playground");
const sourceCompiler = path.join(playgroundDir, "compiler.js");
const sourcePackages = path.join(playgroundDir, "packages");
const args = process.argv.slice(2);
const bundleId = args.find(arg => !arg.startsWith("--")) ?? "local";
const clearOtherBundles = args.includes("--clear-other-bundles");
const bundlesRoot = path.join(devPlaygroundDir, "public", "playground-bundles");
const targetRoot = path.join(bundlesRoot, bundleId);

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

if (clearOtherBundles) {
  for (const entry of await fs.readdir(bundlesRoot)) {
    if (entry !== ".gitignore") {
      await fs.rm(path.join(bundlesRoot, entry), {
        recursive: true,
        force: true,
      });
    }
  }
}

await fs.rm(targetRoot, { recursive: true, force: true });
await fs.mkdir(targetRoot, { recursive: true });
await fs.copyFile(sourceCompiler, path.join(targetRoot, "compiler.js"));
await fs.cp(sourcePackages, targetRoot, { recursive: true });

console.log(`Staged ${bundleId} playground bundle at ${targetRoot}`);
