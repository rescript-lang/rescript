// @ts-check

// Paths of the compiler binaries in the platform package (@rescript/<target>).
// The lookup is synchronous so that tools.cjs, the CommonJS entry of
// `rescript/tools`, can load it; bins.js re-exports these values for ES modules.

const os = require("node:os");
const path = require("node:path");
const { getTarget } = require("./platform.cjs");

const minimumNodeVersion = "22.0.0";

const target = getTarget(process.platform, process.arch, os.release());

if (target === undefined) {
  throw new Error(
    `Platform ${process.platform}-${process.arch} is not supported!`,
  );
}

const binPackageName = `@rescript/${target}`;

/** @type {string} */
let binPackageEntry;
try {
  binPackageEntry = require.resolve(binPackageName);
} catch {
  // First check if we are on an unsupported node version, as that may be the cause for the error.
  checkNodeVersionSupported();

  throw new Error(
    `Package ${binPackageName} not found. Make sure the rescript package is installed correctly.`,
  );
}

// The platform package's entry (bin.js) sits next to its bin directory. It
// exports the same paths for tools that import it directly, such as
// rescript-vscode, so keep the two lists in sync.
const binDir = path.join(path.dirname(binPackageEntry), "bin");

exports.binDir = binDir;
exports.bsc_exe = path.join(binDir, "bsc.exe");
exports.rescript_editor_analysis_exe = path.join(
  binDir,
  "rescript-editor-analysis.exe",
);
exports.rescript_tools_exe = path.join(binDir, "rescript-tools.exe");
exports.rescript_exe = path.join(binDir, "rescript.exe");

function checkNodeVersionSupported() {
  if (
    typeof process !== "undefined" &&
    process.versions != null &&
    process.versions.node != null
  ) {
    const currentVersion = process.versions.node;
    const required = minimumNodeVersion.split(".").map(Number);
    const current = currentVersion.split(".").map(Number);
    if (
      current[0] < required[0] ||
      (current[0] === required[0] && current[1] < required[1]) ||
      (current[0] === required[0] &&
        current[1] === required[1] &&
        current[2] < required[2])
    ) {
      throw new Error(
        `ReScript requires Node.js >=${minimumNodeVersion}, but found ${currentVersion}.`,
      );
    }
  }
}
