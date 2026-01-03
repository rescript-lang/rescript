// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuildLegacy } = setup(import.meta.dirname);

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

const out = await execBuildLegacy();

if (out.status !== 0) {
  assert.fail(out.stdout + out.stderr);
}
