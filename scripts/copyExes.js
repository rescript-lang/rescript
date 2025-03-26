#!/usr/bin/env node
// Copy exes built by dune to platform bin dir

const path = require("path");
const fs = require("fs");
const child_process = require("child_process");
const { duneBinDir } = require("./dune");
const { absolutePath: platformBinDir } = require("#cli/bin_path");

const ninjaDir = path.join(__dirname, "..", "ninja");
const rewatchDir = path.join(__dirname, "..", "rewatch");

if (!fs.existsSync(platformBinDir)) {
  fs.mkdirSync(platformBinDir);
}

function copyExe(dir, exe) {
  const ext = process.platform === "win32" ? ".exe" : "";
  const src = path.join(dir, exe + ext);
  const dest = path.join(platformBinDir, exe + ".exe");

  // For some reason, the copy operation fails in Windows CI if the file already exists.
  if (process.platform === "win32" && fs.existsSync(dest)) {
    fs.rmSync(dest);
  }

  let mode = 0o755;
  if (fs.existsSync(dest)) {
    mode = fs.statSync(dest).mode & 0o777;
    fs.chmodSync(dest, mode | 0o200); // u+w
  }
  try {
    fs.copyFileSync(src, dest);
    if (process.platform !== "win32") {
      fs.chmodSync(dest, mode | 0o200); // u+w
      child_process.execSync(`strip ${dest}`);
    }
  } finally {
    fs.chmodSync(dest, mode);
  }
}

if (process.argv.includes("-all") || process.argv.includes("-compiler")) {
  copyExe(duneBinDir, "rescript");
  copyExe(duneBinDir, "rescript-editor-analysis");
  copyExe(duneBinDir, "rescript-tools");
  copyExe(duneBinDir, "bsc");
  copyExe(duneBinDir, "bsb_helper");
}

if (process.argv.includes("-all") || process.argv.includes("-ninja")) {
  copyExe(ninjaDir, "ninja");
}

if (process.argv.includes("-all") || process.argv.includes("-rewatch")) {
  copyExe(rewatchDir, "rewatch");
}
