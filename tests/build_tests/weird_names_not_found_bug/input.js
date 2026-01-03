import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuildLegacy } = await setup(import.meta.dirname);

const out = await execBuildLegacy();

if (out.stderr !== "") {
  assert.fail(out.stderr);
}

if (!out.stdout.includes(`The module or file Demo can't be found.`)) {
  assert.fail(out.stdout);
}
