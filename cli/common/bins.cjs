// @ts-check

// CommonJS counterpart of bins.js, for `require("rescript/cli/bins")`.
// It locates the platform package with `require.resolve`, so it loads
// synchronously; bins.js uses top-level await, which `require` cannot load.

const path = require("node:path");

const target = `${process.platform}-${process.arch}`;

const supportedPlatforms = [
  "darwin-arm64",
  "darwin-x64",
  "linux-arm64",
  "linux-x64",
  "win32-x64",
];

if (!supportedPlatforms.includes(target)) {
  throw new Error(`Platform ${target} is not supported!`);
}

const binPackageName = `@rescript/${target}`;

/** @type {string} */
let binPackageEntry;
try {
  binPackageEntry = require.resolve(binPackageName);
} catch {
  throw new Error(
    `Package ${binPackageName} not found. Make sure the rescript package is installed correctly.`,
  );
}

// The platform package's entry (bin.js) sits next to its bin directory.
const binDir = path.join(path.dirname(binPackageEntry), "bin");

module.exports = {
  binDir,
  bsc_exe: path.join(binDir, "bsc.exe"),
  rescript_editor_analysis_exe: path.join(
    binDir,
    "rescript-editor-analysis.exe",
  ),
  rescript_tools_exe: path.join(binDir, "rescript-tools.exe"),
  rescript_exe: path.join(binDir, "rescript.exe"),
};
