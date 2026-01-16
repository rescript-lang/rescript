// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

await execClean();
const { stdout, stderr } = await execBuild();

const expectedWarning =
  "Duplicated package: z ./node_modules/z (chosen) vs ./a/node_modules/z in ./a";

const output = stdout + stderr;
if (!output.includes(expectedWarning)) {
  assert.fail(
    `Expected duplicate package warning not found in output.\nExpected: ${expectedWarning}\nActual output:\n${output}`,
  );
}
