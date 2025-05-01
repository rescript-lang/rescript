#!/usr/bin/env node

// @ts-check

// This performs `yarn pack` and retrieves the list of artifact files from the output.
//
// In local dev, invoke it with `--updateArtifactList` to perform a dry run of `yarn pack`
// and recreate `packages/artifacts.txt`.
//
// In CI, the scripts is invoked without options. It then performs `yarn pack` for real,
// recreates the artifact list and verifies that it has no changes compared to the committed state.

import { execSync, spawn } from "node:child_process";
import * as fs from "node:fs/promises";
import * as readline from "node:readline/promises";
import { parseArgs } from "node:util";
import { artifactListFile } from "#dev/paths";

/**
 * @typedef {(
 *   | { "base": string }
 *   | { "location": string }
 *   | { "output": string }
 * )} YarnPackOutputLine
 */

const { values } = parseArgs({
  args: process.argv.slice(2),
  options: {
    updateArtifactList: {
      type: "boolean",
      short: "u",
    },
  },
});

const mode = values.updateArtifactList ? "updateArtifactList" : "package";

const child = spawn(
  "yarn",
  [
    "workspace",
    "rescript",
    "pack",
    "--json",
    mode === "updateArtifactList" ? "--dry-run" : "",
  ].filter(Boolean),
);
const exitCode = new Promise((resolve, reject) => {
  child.once("error", reject);
  child.once("close", code => resolve(code));
});

await fs.unlink(artifactListFile);

for await (const line of readline.createInterface({
  input: child.stdout.setEncoding("utf8"),
  crlfDelay: Number.POSITIVE_INFINITY,
})) {
  /** @type {YarnPackOutputLine} */
  const json = JSON.parse(line);
  if ("location" in json) {
    // Workaround for false positive reports
    // See https://github.com/yarnpkg/berry/issues/6766
    if (json.location.startsWith("_build")) {
      continue;
    }
    await fs.appendFile(artifactListFile, json.location + "\n", "utf8");
  }
}

await exitCode;

if (mode === "package") {
  execSync(`git diff --exit-code ${artifactListFile}`, { stdio: "inherit" });
}
