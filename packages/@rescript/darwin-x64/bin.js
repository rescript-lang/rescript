// @ts-check

import * as path from "node:path";

export const binDir = path.join(import.meta.dirname, "bin");

export const binPaths = {
  bsb_helper_exe: path.join(binDir, "bsb_helper.exe"),
  bsc_exe: path.join(binDir, "bsc.exe"),
  ninja_exe: path.join(binDir, "ninja.exe"),
  rescript_legacy_exe: path.join(binDir, "rescript-legacy.exe"),
  rescript_tools_exe: path.join(binDir, "rescript-tools.exe"),
  rescript_editor_analysis_exe: path.join(
    binDir,
    "rescript-editor-analysis.exe",
  ),
  rescript_exe: path.join(binDir, "rescript.exe"),
};
