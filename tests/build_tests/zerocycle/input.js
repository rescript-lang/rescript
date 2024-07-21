const p = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");
const out = p.spawnSync(rescript_exe, { encoding: "utf8", cwd: __dirname });
assert(out.status === 0);
