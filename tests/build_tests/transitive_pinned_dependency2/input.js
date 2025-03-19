//@ts-check
const child_process = require("node:child_process");
const assert = require("node:assert");
const fs = require("node:fs");
const { rescript_exe } = require("#cli/bin_path");

console.log(
  child_process.execSync(rescript_exe, { encoding: "utf8", cwd: "./a" }),
);

assert(
  !fs.existsSync("./node_modules/c/lib/js/tests/test.mjs"),
  "dev files of module 'c' were built by 'a' even though 'c' is not a pinned dependency of 'a'",
);
