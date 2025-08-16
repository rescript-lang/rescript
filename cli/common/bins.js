// @ts-check

const minimumNodeVersion = "20.11.0";

/**
 * @typedef {import("@rescript/linux-x64")} BinaryModuleExports
 */

const target = `${process.platform}-${process.arch}`;

const supportedPlatforms = [
  "darwin-arm64",
  "darwin-x64",
  "linux-arm64",
  "linux-x64",
  "win32-x64",
];

/** @type {BinaryModuleExports} */
let mod;

if (supportedPlatforms.includes(target)) {
  const binPackageName = `@rescript/${target}`;

  try {
    mod = await import(binPackageName);
  } catch {
    // First check if we are on an unsupported node version, as that may be the cause for the error.
    checkNodeVersionSupported();

    throw new Error(
      `Package ${binPackageName} not found. Make sure the rescript package is installed correctly.`,
    );
  }
} else {
  throw new Error(`Platform ${target} is not supported!`);
}

export const {
  binDir,
  binPaths: {
    bsb_helper_exe,
    bsc_exe,
    ninja_exe,
    rescript_editor_analysis_exe,
    rescript_tools_exe,
    rescript_legacy_exe,
    rescript_exe,
  },
} = mod;

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
