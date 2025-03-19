const p = require("node:child_process");
const { rescript_exe } = require("#cli/bin_path");

p.execSync(rescript_exe);
