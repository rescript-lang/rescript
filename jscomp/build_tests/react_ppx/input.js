//@ts-check
var cp = require("child_process");
var { rescript_exe } = require("rescript/bin_path");

cp.execSync(rescript_exe, { cwd: __dirname });
