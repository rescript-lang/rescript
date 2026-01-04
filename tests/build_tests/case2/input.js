// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild } = setup(import.meta.dirname);

const { stdout } = await execBuild();

if (
  ![
    "Could not initialize build: Implementation and interface have different path names or different cases: `src/X.res` vs `src/x.resi`\n",
    // Windows: path separator
    "Could not initialize build: Implementation and interface have different path names or different cases: `src\\X.res` vs `src\\x.resi`\n",
  ].includes(normalizeNewlines(stdout))
) {
  assert.fail(stdout);
}
