// @ts-check

/**
 * @typedef {import("@rescript/linux-x64")} BinaryModuleExports
 */

const target = `${process.platform}-${process.arch}`;

/** @type {BinaryModuleExports} */
let mod;
try {
  mod = await import(`@rescript/${target}`);
} catch {
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
