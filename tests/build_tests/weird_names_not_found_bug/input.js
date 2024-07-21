const cp = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

const out = cp.spawnSync(rescript_exe, { encoding: "utf8" });
if (out.stderr !== "") {
  assert.fail(out.stderr);
}

if (!out.stdout.includes(`The module or file Demo can't be found.`)) {
  assert.fail(out.stdout);
}
