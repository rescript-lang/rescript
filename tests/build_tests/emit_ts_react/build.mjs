import assert from "node:assert/strict";
import { fileURLToPath } from "node:url";
import { setup } from "../../../lib_dev/process.js";

const { execBuildOrThrow, execClean, node } = setup(import.meta.dirname);
const tsc = fileURLToPath(
  new URL("../../../node_modules/typescript/bin/tsc", import.meta.url),
);

export async function build() {
  await execClean();
  await execBuildOrThrow();
  for (const config of [
    "tsconfig.json",
    "contracts/verification-tsconfig.json",
  ]) {
    const result = await node(tsc, ["--project", config, "--pretty", "false"]);
    assert.equal(result.status, 0, result.stdout + result.stderr);
  }
}
