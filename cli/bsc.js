#!/usr/bin/env node

// @ts-check

import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { dirname, join } from "node:path";

import { bsc_exe } from "./common/bins.js";
import { runtimePath } from "./common/runtime.js";

/**
 * Walk up from `startDir` until a directory containing `rescript.json`
 * is found. Returns null if none is found.
 * @param {string} startDir
 * @returns {string | null}
 */
function findProjectRoot(startDir) {
  let dir = startDir;
  while (true) {
    if (existsSync(join(dir, "rescript.json"))) {
      return dir;
    }
    const parent = dirname(dir);
    if (parent === dir) return null;
    dir = parent;
  }
}

const delegate_args = process.argv.slice(2);
if (!delegate_args.includes("-runtime-path")) {
  delegate_args.push("-runtime-path", runtimePath);
}
if (!delegate_args.includes("-bs-project-root")) {
  const projectRoot = findProjectRoot(process.cwd());
  if (projectRoot !== null) {
    delegate_args.push("-bs-project-root", projectRoot);
  }
}

try {
  execFileSync(bsc_exe, delegate_args, { stdio: "inherit" });
} catch (e) {
  if (e.code === "ENOENT") {
    console.error(String(e));
  }
  process.exit(2);
}
