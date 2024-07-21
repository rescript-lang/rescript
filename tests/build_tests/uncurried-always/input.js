//@ts-check
const cp = require("node:child_process");
const { rescript_exe } = require("#cli/bin_path");

cp.execSync(`${rescript_exe} clean`, { cwd: __dirname });
cp.execSync(`${rescript_exe}`, { cwd: __dirname });
