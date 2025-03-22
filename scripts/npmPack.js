#!/usr/bin/env node
// @ts-check

// TODO: Use `yarn pack --json` instead.

// This performs `npm pack` and retrieves the list of artifact files from the output.
//
// In local dev, invoke it with `-updateArtifactList` to perform a dry run of `npm pack`
// and recreate `packages/artifacts.txt`.
// The exes for all platforms will then be included in the list, even if not present locally.
//
// In CI, the scripts is invoked without options. It then performs `npm pack` for real,
// recreates the artifact list and verifies that it has no changes compared to the committed state.

/**
 * @typedef {{
 *   path: string,
 *   size: number,
 *   mode: number,
 * }} PackOutputFile
 *
 * @typedef {{
 *   files: PackOutputFile[],
 *   entryCount: number,
 *   bundled: unknown[],
 * }} PackOutputEntry
 *
 * @typedef {[PackOutputEntry]} PackOutput
 */

const { spawnSync, execSync } = require("child_process");
const path = require("path");
const fs = require("fs");

const mode = process.argv.includes("-updateArtifactList")
  ? "updateArtifactList"
  : "package";

const rootPath = path.join(__dirname, "..");
const fileListPath = path.join(rootPath, "packages", "artifacts.txt");

const output = spawnSync(
  "npm pack --json" + (mode === "updateArtifactList" ? " --dry-run" : ""),
  {
    cwd: rootPath,
    encoding: "utf8",
    shell: true,
  },
).stdout;

/** @type {PackOutput} */
const parsedOutput = JSON.parse(output);
let filePaths = parsedOutput[0].files.map(file => file.path);

if (mode === "updateArtifactList") {
  filePaths = Array.from(new Set(filePaths.concat(getFilesAddedByCI())));
}

filePaths.sort();
fs.writeFileSync(fileListPath, filePaths.join("\n"));

if (mode === "package") {
  execSync(`git diff --exit-code ${fileListPath}`, { stdio: "inherit" });
}

function getFilesAddedByCI() {
  const platforms = ["darwin", "darwinarm64", "linux", "linuxarm64", "win32"];
  const exes = [
    "bsb_helper.exe",
    "bsc.exe",
    "ninja.exe",
    "rescript.exe",
    "rescript-editor-analysis.exe",
    "rescript-tools.exe",
    "rewatch.exe",
  ];

  const files = ["ninja.COPYING"];

  for (let platform of platforms) {
    for (let exe of exes) {
      files.push(`${platform}/${exe}`);
    }
  }

  return files;
}
