// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);
const diagnosticsDir = path.join(
  import.meta.dirname,
  "lib",
  "bs",
  "src",
  "Main.debug-ir",
);

await execClean();

try {
  await execBuildOrThrow();

  const artifacts = (await fs.readdir(diagnosticsDir)).sort();
  assert.ok(artifacts.includes("01-lam-initial.lam"));
  assert.ok(artifacts.some(name => name.endsWith("-lam-groups.lambda")));
  assert.ok(artifacts.some(name => name.endsWith("-js-initial.jsx")));

  const indexes = artifacts.map(name => Number.parseInt(name.slice(0, 2), 10));
  assert.deepEqual(
    indexes,
    Array.from({ length: artifacts.length }, (_, index) => index + 1),
  );

  const staleArtifact = path.join(diagnosticsDir, "99-stale.lam");
  await fs.writeFile(staleArtifact, "stale");
  const source = path.join(import.meta.dirname, "src", "Main.res");
  const now = new Date();
  await fs.utimes(source, now, now);
  await execBuildOrThrow();
  await assert.rejects(fs.access(staleArtifact));
} finally {
  await execClean();
}

await assert.rejects(fs.access(diagnosticsDir));
