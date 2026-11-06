// @ts-check

// rescript/cli/bins computes these paths itself; they are exported for tools
// that import this file directly, such as rescript-vscode.

import * as path from "node:path";

export const binDir = path.join(import.meta.dirname, "bin");

export const binPaths = {
  bsc_exe: path.join(binDir, "bsc.exe"),
  rescript_tools_exe: path.join(binDir, "rescript-tools.exe"),
  rescript_editor_analysis_exe: path.join(
    binDir,
    "rescript-editor-analysis.exe",
  ),
  rescript_exe: path.join(binDir, "rescript.exe"),
};
