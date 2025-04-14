#!/usr/bin/env node

// @ts-check

// NOTE:
//   We cannot use `yarn pack` since we need to set our OCaml binaries executable.
//   Yarn (Berry) only allow `bin` to be executable, wouldn't preserve permission bits.

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

import { execSync, spawnSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { projectDir } from "#dev/paths";

const mode = process.argv.includes("-updateArtifactList")
  ? "updateArtifactList"
  : "package";

const fileListPath = path.join(projectDir, "packages", "artifacts.txt");

const output = spawnSync(
  `npm pack --json${mode === "updateArtifactList" ? " --dry-run" : ""}`,
  {
    cwd: projectDir,
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

  for (const platform of platforms) {
    for (const exe of exes) {
      files.push(`${platform}/${exe}`);
    }
  }

  return files;
}
