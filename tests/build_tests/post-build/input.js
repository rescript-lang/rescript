// @ts-check

import * as assert from "node:assert";
import { setupWithUrl } from "#dev/process";

const { execBuild } = setupWithUrl(import.meta.url);

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

const out = await execBuild();

if (out.status !== 0) {
  assert.fail(out.stdout + out.stderr);
}
