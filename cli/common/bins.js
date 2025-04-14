// @ts-check

/**
 * @typedef {{
 *   platformDir: string,
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

/** @type {BinaryPaths} */
let binPaths;
try {
  binPaths = await import(`@rescript/${target}/paths`);
} catch (err) {
  console.error(`Platform ${target} is not supported!`);
  throw err;
}

export const {
  platformDir,
  bsb_helper_exe,
  bsc_exe,
  ninja_exe,
  rescript_editor_analysis_exe,
  rescript_tools_exe,
  rescript_exe,
  rewatch_exe,
} = binPaths;
