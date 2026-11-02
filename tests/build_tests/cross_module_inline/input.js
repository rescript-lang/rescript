// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);
const source = path.join(import.meta.dirname, "src", "A.res");
const output = path.join(import.meta.dirname, "src", "B.js");
const cmj = path.join(import.meta.dirname, "lib", "bs", "src", "A.cmj");
const original = await fs.readFile(source, "utf8");

await execClean();
try {
  await execBuildOrThrow();
  assert.match(await fs.readFile(output, "utf8"), /let result = 2;/);
  await fs.access(cmj);
  await execClean();
  await assert.rejects(fs.access(cmj));
  await execBuildOrThrow();

  // The public type stays the same, but the saved inline body changes.
  await fs.writeFile(source, original.replace("x + 1", "x + 2"));
  await execBuildOrThrow();
  assert.match(await fs.readFile(output, "utf8"), /let result = 3;/);

  // Removing the marker restores a normal call.
  await fs.writeFile(source, original.replace("@inline(crossModule)\n", ""));
  await execBuildOrThrow();
  assert.match(await fs.readFile(output, "utf8"), /A\.bump\(1\)/);
} finally {
  await fs.writeFile(source, original);
  await execClean();
}
