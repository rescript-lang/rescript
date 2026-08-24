#!/usr/bin/env node

// @ts-check

// Copy the rewatch exe built by cargo to the platform bin dir.
// The dune-built compiler binaries are copied by dune promotion instead
// (see compiler/sync/dune).

import * as child_process from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";
import { parseArgs } from "node:util";
import { binDir } from "#cli/bins";
import { rewatchDir } from "#dev/paths";

const args = parseArgs({
  args: process.argv.slice(2),
  options: {
    all: {
      type: "boolean",
    },
    rewatch: {
      type: "boolean",
    },
  },
});

const shouldCopyRewatch = args.values.all || args.values.rewatch;

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
