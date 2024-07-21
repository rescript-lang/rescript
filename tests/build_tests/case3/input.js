//@ts-check

const p = require("node:child_process");
const fs = require("node:fs");
const path = require("node:path");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");
p.spawnSync(`${rescript_exe} clean && ${rescript_exe} build`, {
  encoding: "utf8",
  cwd: __dirname,
});

const o = fs.readFileSync(path.join(__dirname, "src", "hello.bs.js"), "ascii");
assert.ok(/HelloGen\.f/.test(o));
