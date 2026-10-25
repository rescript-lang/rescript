// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { runtimePath } from "#cli/runtime";
import { setup } from "#dev/process";

// Set runtime path for rewatch to find
process.env.RESCRIPT_RUNTIME = runtimePath;

const { execBuild, execClean } = setup(import.meta.dirname);

// Without core.symlinks, Git on Windows checks the links out as plain files.
// CI enables it, so only local checkouts may skip.
const link = await fs.lstat(
  path.join(import.meta.dirname, "node_modules", "a"),
);
if (!link.isSymbolicLink()) {
  if (process.env.CI) {
    assert.fail("node_modules/a was not checked out as a symlink");
  }
  console.log("Skipping test: symlinks are not checked out as symlinks");
  process.exit(0);
}

await execClean();
const { stderr: rawStderr } = await execBuild();
// Windows joins the package paths with backslashes.
const stderr = rawStderr.replaceAll("\\", "/");

// c reaches a and b through node_modules symlinks and counts once; z has two
// copies, node_modules/z and a/node_modules/z, the one duplicate to report.
const expectedWarning =
  "Duplicated package: z ./node_modules/z (chosen) vs ./a/node_modules/z in ./a";

if (!stderr.includes(expectedWarning)) {
  assert.fail(
    `Expected duplicate package warning not found in stderr.\nExpected: ${expectedWarning}\nActual stderr:\n${stderr}`,
  );
}
