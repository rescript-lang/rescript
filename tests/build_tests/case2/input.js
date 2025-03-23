// @ts-check

import * as assert from "node:assert";
import { setupWithUrl } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild } = setupWithUrl(import.meta.url);

const { stderr } = await execBuild();

if (
  ![
    "Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/X vs src/x\n",
    // Windows: path separator
    "Error: Invalid bsconfig.json implementation and interface have different path names or different cases src\\X vs src\\x\n",
    // Linux: files are parsed in different order
    "Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/x vs src/X\n",
  ].includes(normalizeNewlines(stderr))
) {
  assert.fail(stderr);
}
