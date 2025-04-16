// @ts-check

import * as fs from "node:fs";

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

const binPaths = mod.binPaths;
// Compiler use symbolic links in the development process
if (fs.lstatSync(binPaths.rescript_exe).isSymbolicLink()) {
  for (const [key, binPath] of Object.entries(binPaths)) {
    try {
      binPaths[/** @type {keyof BinaryPaths} */ (key)] =
        fs.realpathSync(binPath);
    } catch {
      // Cannot populate the realpath of a binary, some tests may fails.
      //
      // But we don't report error here,
      // so pass even some binaries are not built yet.
    }
  }
}

export const platformDir = mod.binDir;
export const {
  bsb_helper_exe,
  bsc_exe,
  ninja_exe,
  rescript_editor_analysis_exe,
  rescript_tools_exe,
  rescript_exe,
  rewatch_exe,
} = binPaths;
