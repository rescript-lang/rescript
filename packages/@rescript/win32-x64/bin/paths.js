// @ts-check

import * as path from "node:path";

export const platformDir = import.meta.dirname;

export const bsc_exe = path.join(platformDir, "bsc.exe");

export const ninja_exe = path.join(platformDir, "ninja.exe");

export const rescript_exe = path.join(platformDir, "rescript.exe");

export const rescript_tools_exe = path.join(platformDir, "rescript-tools.exe");

export const rescript_editor_analysis_exe = path.join(
  platformDir,
  "rescript-editor-analysis.exe",
);

export const rewatch_exe = path.join(platformDir, "rewatch.exe");
