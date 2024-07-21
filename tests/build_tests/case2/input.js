const p = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");
const { normalizeNewlines } = require("../utils.js");

const o = p.spawnSync(rescript_exe, { encoding: "utf8", cwd: __dirname });

if (
  ![
    "Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/X vs src/x\n",
    // Windows: path separator
    "Error: Invalid bsconfig.json implementation and interface have different path names or different cases src\\X vs src\\x\n",
    // Linux: files are parsed in different order
    "Error: Invalid bsconfig.json implementation and interface have different path names or different cases src/x vs src/X\n",
  ].includes(normalizeNewlines(o.stderr))
) {
  assert.fail(o.stderr);
}
