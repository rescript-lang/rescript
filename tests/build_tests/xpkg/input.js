const p = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");
try {
  const output = p.spawnSync(`${rescript_exe} build -regen`, {
    shell: true,
    encoding: "utf8",
  });

  assert.ok(output.stderr.match(/reserved package name/));
} finally {
}
