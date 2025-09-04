#!/usr/bin/env node

// @ts-check

// Copy exes built by dune to platform bin dir

import * as child_process from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { parseArgs } from "node:util";
import { binDir } from "#cli/bins";
import { compilerBinDir, ninjaDir, rewatchDir } from "#dev/paths";

const args = parseArgs({
  args: process.argv.slice(2),
  options: {
    all: {
      type: "boolean",
    },
    compiler: {
      type: "boolean",
    },
    ninja: {
      type: "boolean",
    },
    rewatch: {
      type: "boolean",
    },
  },
});

const shouldCopyCompiler = args.values.all || args.values.compiler;
const shouldCopyNinja = args.values.all || args.values.ninja;
const shouldCopyRewatch = args.values.all || args.values.rewatch;

if (shouldCopyCompiler) {
  copyExe(compilerBinDir, "rescript-legacy");
  copyExe(compilerBinDir, "rescript-editor-analysis");
  copyExe(compilerBinDir, "rescript-tools");
  copyExe(compilerBinDir, "bsc");
  copyExe(compilerBinDir, "bsb_helper");
}

if (shouldCopyNinja) {
  copyExe(ninjaDir, "ninja");
}

if (shouldCopyRewatch) {
  copyExe(path.join(rewatchDir, "target", "release"), "rescript");
}

/**
 * @param {string} dir
 * @param {string} exe
 * @param {string | undefined} renamed
 */
function copyExe(dir, exe, renamed) {
  const ext = process.platform === "win32" ? ".exe" : "";
  const src = path.join(dir, exe + ext);
  const dest = path.join(binDir, `${renamed ?? exe}.exe`);

  // Skip if the source binary was not built (e.g., skipped in a profile).
  if (!fs.existsSync(src)) {
    return;
  }

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
