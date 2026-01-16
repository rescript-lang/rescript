// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";
import { runtimePath } from "#cli/runtime";

// Set runtime path for rewatch to find
process.env.RESCRIPT_RUNTIME = runtimePath;

const { execBuild, execClean } = setup(import.meta.dirname);

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

await execClean();
const { stderr } = await execBuild();

const expectedWarning =
  "Duplicated package: z ./node_modules/z (chosen) vs ./a/node_modules/z in ./a";

if (!stderr.includes(expectedWarning)) {
  assert.fail(
    `Expected duplicate package warning not found in stderr.\nExpected: ${expectedWarning}\nActual stderr:\n${stderr}`,
  );
}
