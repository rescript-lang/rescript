const { spawnSync } = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

const output = spawnSync(rescript_exe, { encoding: "utf8" });
assert(
  /^Warning: bsconfig.json is deprecated. Migrate it to rescript.json/.test(
    output.stdout,
  ),
);
