#!/usr/bin/env node

// @ts-check

import { execFileSync } from "node:child_process";

import { bsc_exe } from "./common/bins.js";
import { runtimePath } from "./common/runtime.js";

const delegate_args = process.argv.slice(2);
if (!delegate_args.includes("-runtime-path")) {
  delegate_args.push("-runtime-path", runtimePath);
}

try {
  execFileSync(bsc_exe, delegate_args, { stdio: "inherit" });
} catch (e) {
  if (e.code === "ENOENT") {
    console.error(String(e));
  }
  process.exit(2);
}
