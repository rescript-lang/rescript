const child_process = require("node:child_process");
const assert = require("node:assert");

const { rescript_exe } = require("#cli/bin_path");

assert.throws(
  () => {
    const _output = child_process.execSync(`${rescript_exe} build -regen`, {
      cwd: __dirname,
      encoding: "utf8",
    });
  },
  err => {
    if (err.message.match(/detected two module formats/)) {
      return true;
    }
    return false;
  },
);
