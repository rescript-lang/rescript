const p = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

const o1 = p.spawnSync(rescript_exe, ["build"], {
  encoding: "utf8",
  cwd: __dirname,
});

const first_message = o1.stdout
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110");

if (!first_message) {
  assert.fail(o1.stdout);
}

// Second build using -warn-error +110
const o2 = p.spawnSync(rescript_exe, ["build", "-warn-error", "+110"], {
  encoding: "utf8",
  cwd: __dirname,
});

const second_message = o2.stdout
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Warning number 110 (configured as error)");

if (!second_message) {
  assert.fail(o2.stdout);
}

// Third build, without -warn-error +110
// The result should not be a warning as error
const o3 = p.spawnSync(rescript_exe, ["build"], {
  encoding: "utf8",
  cwd: __dirname,
});

const third_message = o3.stdout
  .split("\n")
  .map(s => s.trim())
  .find(s => s === "Dependency Finished");

if (!third_message) {
  assert.fail(o3.stdout);
}

var cleanup = p.spawnSync(rescript_exe, ["clean"], {
  encoding: "utf8",
  cwd: __dirname,
});
