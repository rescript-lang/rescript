import * as assert from "node:assert";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild } = setup(import.meta.dirname);

const { stderr } = await execBuild();

const normalized = normalizeNewlines(stderr);
const expectedError =
  "Implementation and interface have different path names or different cases:";
const expectedPaths =
  normalized.includes("`src/demo.res` vs `src/Demo.resi`") ||
  normalized.includes("`src\\demo.res` vs `src\\Demo.resi`"); // Windows

assert.ok(
  normalized.includes(expectedError) && expectedPaths,
  `Expected error about mismatched case, got: ${stderr}`,
);
