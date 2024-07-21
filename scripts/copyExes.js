#!/usr/bin/env node

// @ts-check

// Copy exes built by dune to platform bin dir

const path = require("node:path");
const fs = require("node:fs");
const child_process = require("node:child_process");
const { platformDir } = require("#cli/bin_path");
const { compilerBinDir, ninjaDir, rewatchDir } = require("#dev/paths");

fs.mkdirSync(platformDir, { recursive: true });

/**
 * @param {string} dir
 * @param {string} exe
 */
function copyExe(dir, exe) {
  const ext = process.platform === "win32" ? ".exe" : "";
  const src = path.join(dir, exe + ext);
  const dest = path.join(platformDir, `${exe}.exe`);

  // For some reason, the copy operation fails in Windows CI if the file already exists.
  if (process.platform === "win32" && fs.existsSync(dest)) {
    fs.rmSync(dest);
  }

  fs.copyFileSync(src, dest);

  if (process.platform !== "win32") {
    child_process.execSync(`strip ${dest}`);
  }
}

if (process.argv.includes("-all") || process.argv.includes("-compiler")) {
  copyExe(compilerBinDir, "rescript");
  copyExe(compilerBinDir, "rescript-editor-analysis");
  copyExe(compilerBinDir, "rescript-tools");
  copyExe(compilerBinDir, "bsc");
  copyExe(compilerBinDir, "bsb_helper");
}

if (process.argv.includes("-all") || process.argv.includes("-ninja")) {
  copyExe(ninjaDir, "ninja");
}

if (process.argv.includes("-all") || process.argv.includes("-rewatch")) {
  copyExe(rewatchDir, "rewatch");
}
