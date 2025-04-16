// @ts-check

/**
 * @typedef {{
 *   binDir: string,
 *   binPaths: BinaryPaths,
 * }} BinaryModuleExports
 *
 * @typedef {{
 *   bsb_helper_exe: string,
 *   bsc_exe: string,
 *   ninja_exe: string,
 *   rescript_exe: string,
 *   rescript_tools_exe: string,
 *   rescript_editor_analysis_exe: string,
 *   rewatch_exe: string,
 * }} BinaryPaths
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
  binDir: platformDir,
  binPaths: {
    bsb_helper_exe,
    bsc_exe,
    ninja_exe,
    rescript_editor_analysis_exe,
    rescript_tools_exe,
    rescript_exe,
    rewatch_exe,
  },
} = mod;
