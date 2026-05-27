#!/usr/bin/env node

// @ts-check

// Publishes a mutable PR playground compiler bundle to Cloudflare R2.
// The bundle is addressed by PR number, so each PR keeps one latest preview.

import * as fs from "node:fs";
import * as path from "node:path";

import {
  exec,
  playgroundDir,
  playgroundPackagesDir,
} from "./common.mjs";

const previewId = process.env.PLAYGROUND_PREVIEW_ID;
if (!previewId || !/^pr-[0-9]+$/.test(previewId)) {
  throw new Error("PLAYGROUND_PREVIEW_ID must look like pr-123");
}

const rcloneOpts = [
  "--stats 5",
  "--checkers 5000",
  "--transfers 8",
  "--buffer-size 128M",
  "--s3-no-check-bucket",
  "--s3-chunk-size 128M",
  "--s3-upload-concurrency 8",
].join(" ");

const remote = process.env.RCLONE_REMOTE || "rescript";
const bucket = "cdn-assets";
const tmpDir = path.join(playgroundDir, ".tmp", "preview");
const artifactsDir = path.join(tmpDir, previewId);
const target = `${remote}:${bucket}/dev-playground-bundles/${previewId}/bundle`;

fs.rmSync(tmpDir, { recursive: true, force: true });
fs.mkdirSync(artifactsDir, { recursive: true });

console.log("Copying compiler.js");
fs.copyFileSync(
  path.join(playgroundDir, "compiler.js"),
  path.join(artifactsDir, "compiler.js"),
);

console.log("Copying packages");
fs.cpSync(playgroundPackagesDir, artifactsDir, { recursive: true });

console.log(`Uploading playground preview ${previewId}`);
exec(`rclone sync ${rcloneOpts} --fast-list \\
  "${artifactsDir}" \\
  "${target}"
`);

console.log(`Uploaded playground preview to ${target}`);
