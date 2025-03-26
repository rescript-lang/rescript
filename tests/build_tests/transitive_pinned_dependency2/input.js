//@ts-check
var child_process = require("child_process");
var assert = require("assert");
var fs = require("fs");
var { rescript_exe } = require("#cli/bin_path");

console.log(
  child_process.execSync(`${rescript_exe} clean`, {
    encoding: "utf8",
    cwd: "./a",
  }),
);
console.log(
  child_process.execSync(rescript_exe, { encoding: "utf8", cwd: "./a" }),
);

assert(
  !fs.existsSync("./node_modules/c/lib/js/tests/test.mjs"),
  "dev files of module 'c' were built by 'a' even though 'c' is not a pinned dependency of 'a'",
);
