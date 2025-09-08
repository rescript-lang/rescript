#!/usr/bin/env node

// @ts-check

import { spawn } from "node:child_process";
import * as fs from "node:fs/promises";
import * as readline from "node:readline/promises";
import { artifactListFile } from "#dev/paths";

/**
 * @typedef {(
 *   | { "base": string }
 *   | { "location": string }
 *   | { "output": string }
 * )} YarnPackOutputLine
 */

/**
 * @param {string} pkg
 */
async function getArtifacts(pkg) {
  const args = ["workspace", pkg, "pack", "--json", "--dry-run"];

  const files = [];

  const child = spawn("yarn", args);

  const exitCode = new Promise((resolve, reject) => {
    child.once("error", reject);
    child.once("close", code => resolve(code));
  });

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

      files.push(json.location);
    }
  }

  await exitCode;

  return files;
}

/** @type {Record<string, string[]>} */
const artifactsPerPackage = {};

for (const pkg of ["rescript", "@rescript/runtime"]) {
  artifactsPerPackage[pkg] = await getArtifacts(pkg);
}

await fs.writeFile(
  artifactListFile,
  JSON.stringify(artifactsPerPackage, null, 2),
);
