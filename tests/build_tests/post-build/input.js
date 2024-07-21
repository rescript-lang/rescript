const child_process = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

const out = child_process.spawnSync(rescript_exe, { encoding: "utf8" });

if (out.status !== 0) {
  assert.fail(out.stdout + out.stderr);
}
