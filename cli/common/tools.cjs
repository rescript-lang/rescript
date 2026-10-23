// @ts-check

// Public entry point `rescript/tools` for CommonJS: the path of the
// rescript-tools binary, which RescriptTools.binaryPath binds to.

exports.binaryPath = require("./bins.cjs").rescript_tools_exe;
