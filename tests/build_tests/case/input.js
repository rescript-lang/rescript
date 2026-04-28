import * as assert from "node:assert";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild } = setup(import.meta.dirname);

const { stderr } = await execBuild();

if (
  ![
    "Could not initialize build: Implementation and interface have different path names or different cases: `src/demo.res` vs `src/Demo.resi`\n",
    // Windows: path separator
    "Could not initialize build: Implementation and interface have different path names or different cases: `src\\demo.res` vs `src\\Demo.resi`\n",
  ].includes(normalizeNewlines(stderr))
) {
  assert.fail(stderr);
}
