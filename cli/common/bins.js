// @ts-check

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
