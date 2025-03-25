// @ts-check

import * as path from "node:path";

/**
 * For compatibility reasons, if the architecture is x64, omit it from the bin directory name.
 * So we'll have "darwin", "linux" and "win32" for x64 arch,
 * but "darwinarm64" and "linuxarm64" for arm64 arch.
 * Also, we do not have Windows ARM binaries yet. But the x64 binaries do work on Windows 11 ARM.
 * So omit the architecture for Windows, too.
 */
export const platformName =
  process.arch === "x64" || process.platform === "win32"
    ? process.platform
    : process.platform + process.arch;

export const platformDir = path.resolve(
  import.meta.dirname,
  "..",
  "..",
  platformName,
);

export const bsc_exe = path.join(platformDir, "bsc.exe");

export const ninja_exe = path.join(platformDir, "ninja.exe");

export const rescript_exe = path.join(platformDir, "rescript.exe");

export const rescript_tools_exe = path.join(platformDir, "rescript-tools.exe");

export const rescript_editor_analysis_exe = path.join(
  platformDir,
  "rescript-editor-analysis.exe",
);

export const rewatch_exe = path.join(platformDir, "rewatch.exe");
