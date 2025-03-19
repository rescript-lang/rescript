// @ts-check

const child_process = require("node:child_process");
const fs = require("node:fs");
const path = require("node:path");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

console.log(child_process.execSync(rescript_exe, { encoding: "utf8" }));

const content = fs.readFileSync(
  path.join(__dirname, "lib", "bs", ".sourcedirs.json"),
  "utf-8",
);

assert(JSON.parse(content).dirs.some(x => x.includes("📕annotation")));
