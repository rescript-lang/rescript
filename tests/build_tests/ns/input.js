const child_process = require("node:child_process");
const { rescript_exe } = require("#cli/bin_path");

child_process.execSync(rescript_exe, { cwd: __dirname });
