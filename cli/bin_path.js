// @ts-check

const path = require("node:path");

/**
 * For compatibility reasons, if the architecture is x64, omit it from the bin directory name.
 * So we'll have "darwin", "linux" and "win32" for x64 arch,
 * but "darwinarm64" and "linuxarm64" for arm64 arch.
 * Also, we do not have Windows ARM binaries yet. But the x64 binaries do work on Windows 11 ARM.
 * So omit the architecture for Windows, too.
 */
const platformName =
  process.arch === "x64" || process.platform === "win32"
    ? process.platform
    : process.platform + process.arch;

/**
 * @type {string}
 */
const platformDir = path.resolve(__dirname, "..", platformName);

/**
 * @type {string}
 */
const bsc_exe = path.join(platformDir, "bsc.exe");

/**
 * @type {string}
 */
const ninja_exe = path.join(platformDir, "ninja.exe");

/**
 * @type {string}
 */
const rescript_exe = path.join(platformDir, "rescript.exe");

/**
 * @type {string}
 */
const rescript_tools_exe = path.join(platformDir, "rescript-tools.exe");

/**
 * @type {string}
 */
const rescript_editor_analysis_exe = path.join(
  platformDir,
  "rescript-editor-analysis.exe",
);

/**
 * @type {string}
 */
const rewatch_exe = path.join(platformDir, "rewatch.exe");

exports.platformName = platformName;
exports.platformDir = platformDir;
exports.bsc_exe = bsc_exe;
exports.ninja_exe = ninja_exe;
exports.rescript_exe = rescript_exe;
exports.rescript_tools_exe = rescript_tools_exe;
exports.rescript_editor_analysis_exe = rescript_editor_analysis_exe;
exports.rewatch_exe = rewatch_exe;
