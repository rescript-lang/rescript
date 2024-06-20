var cp = require("child_process");
var assert = require("assert");
var { rescript_exe } = require("#cli/bin_path");

var out = cp.spawnSync(rescript_exe, { encoding: "utf8" });
if (out.stderr !== "") {
  assert.fail(out.stderr);
}

if (!out.stdout.includes(`The module or file Demo can't be found.`)) {
  assert.fail(out.stdout);
}
