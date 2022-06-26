var child_process = require("child_process");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;

assert.throws(
  () => {
    child_process.execSync(`${rescript_exe} clean -with-deps && ${rescript_exe} build`, {
      cwd: __dirname,
      encoding: "utf8",
    });
  },
  (err) => {
    if (
      err.stdout.match(/Unbound value Liba.Priv.v/) ||
      err.stdout.match(/Liba.Priv is an alias/)
    ) {
      // error message changed for 4.06
      return true;
    }
    return false;
  }
);
