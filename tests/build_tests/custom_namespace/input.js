const child_process = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

child_process.execSync(`${rescript_exe} clean && ${rescript_exe} build`, {
  cwd: __dirname,
});

const x = require("./src/demo.bs.js");
assert.equal(x.v, 42);
