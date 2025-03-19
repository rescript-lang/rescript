//@ts-check
const cp = require("node:child_process");
const assert = require("node:assert");
const fs = require("node:fs");
const path = require("node:path");
const { rescript_exe } = require("#cli/bin_path");

const output = cp.spawnSync(rescript_exe, { encoding: "utf8", shell: true });

assert(/dependency cycle/.test(output.stdout));

const compilerLogFile = path.join(__dirname, "lib", "bs", ".compiler.log");
const compilerLog = fs.readFileSync(compilerLogFile, "utf8");
assert(/dependency cycle/.test(compilerLog));
