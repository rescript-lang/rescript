//@ts-check

const cp = require("node:child_process");
const assert = require("node:assert");
const os = require("node:os");
const { rescript_exe } = require("#cli/bin_path");

const out = cp.spawnSync(rescript_exe, {
  cwd: __dirname,
  encoding: "utf8",
});

if (out.stdout !== "") {
  assert.fail(out.stdout);
} else {
  assert.equal(
    out.stderr,
    [
      'File "bsconfig.json", line 1',
      "Error: package weird not found or built",
      "- Did you install it?",
      "",
    ].join(os.EOL),
  );
}
