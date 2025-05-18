#!/usr/bin/env node

// @ts-check

// This script will publish the compiler.js bundle / packages cmij.js files to our Cloudclare R2.
// The target folder on R2 bucket will be the compiler.js' version number.
// This script requires `rclone` and `zstd` to be installed.

import * as fs from "node:fs";
import * as path from "node:path";
import * as readline from "node:readline/promises";
import { createRequire } from "node:module";
import semver from "semver";

import {
  exec,
  playgroundDir,
  playgroundPackagesDir,
} from "./common.mjs";

const require = createRequire(import.meta.url);
// @ts-ignore
const { rescript_compiler } = require('../compiler.js');

/**
 * @type {number}
 */
const version = rescript_compiler.make().rescript.version;
const tag = "v" + version;

console.log("Uploading playground artifacts for %s", tag);
if (!process.env.CI) {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  const answer = await rl.question("Do you want to proceed? [y/N]: ");
  rl.close();

  if (answer.toLowerCase() !== "y") {
    console.log("Cancelled");
    process.exit(1);
  }
}

const rcloneOpts = (process.env.CI
  ? [
    "--stats 5",
    "--checkers 5000",
    "--transfers 8",
    "--buffer-size 128M",
    "--s3-chunk-size 128M",
    "--s3-upload-concurrency 8",
  ]
  : [
    "--progress",
    "--checkers 5000",
    "--transfers 16",
    "--buffer-size 128M",
    "--s3-chunk-size 128M",
    "--s3-upload-concurrency 16",
  ]).join(" ");

const remote = process.env.RCLONE_REMOTE || "rescript";
const bucket = "cdn-assets";

// Create a temporary directory for bundling
const tmpDir = path.join(playgroundDir, ".tmp");
const artifactsDir = path.join(tmpDir, tag);
const archivePath = path.join(tmpDir, `${tag}.tar.zst`);
fs.rmSync(tmpDir, { recursive: true, force: true });
fs.mkdirSync(artifactsDir, { recursive: true });

console.log("Copying compiler.js");
fs.copyFileSync(
  path.join(playgroundDir, "compiler.js"),
  path.join(artifactsDir, "compiler.js"),
);

console.log("Copying packages");
fs.cpSync(playgroundPackagesDir, artifactsDir, { recursive: true });

// Create tar.zst archive
console.log("Creating archive...");
exec(`tar \\
  --use-compress-program="zstd -T0 --adapt --exclude-compressed" \\
  -cf "${archivePath}" \\
  -C "${artifactsDir}" .
`);

// TODO: This will no longer be needed after the KeyCDN to Cloudflare transition is complete.
console.log(`Uploading v${version} artifacts...`);
exec(`rclone sync ${rcloneOpts} --fast-list \\
  "${artifactsDir}" \\
  "${remote}:${bucket}/${tag}"
`);

console.log("Uploading archive...");
exec(`rclone copyto ${rcloneOpts} \\
  "${archivePath}" \\
  "${remote}:${bucket}/playground-bundles/${tag}.tar.zst"
`);

console.log("Update versions.json");
{
  const bundles = exec(
    `rclone lsf --include="*.tar.zst" rescript:cdn-assets/playground-bundles`,
    { stdio: ['ignore', 'pipe', 'pipe'] },
  ).trimEnd().split("\n");

  /** @type {string[]} */
  let playgroundVersions = [];
  for (const bundle of bundles) {
    playgroundVersions.push(bundle.replace(".tar.zst", ""));
  }
  playgroundVersions = semver.sort(playgroundVersions);

  console.log('Versions: ', playgroundVersions);

  const versionsPath = path.join(tmpDir, "versions.json");
  fs.writeFileSync(
    versionsPath,
    JSON.stringify(playgroundVersions),
  );
  exec(`rclone copyto ${rcloneOpts} \\
    "${versionsPath}" \\
    "${remote}:${bucket}/playground-bundles/versions.json"
  `);
}
