#!/usr/bin/env node

// @ts-check

import { execFileSync } from "node:child_process";

import { bsc_exe } from "./common/bins.js";
import { stdlibDir } from "./common/stdlib.js";

const delegate_args = process.argv.slice(2);

try {
  execFileSync(bsc_exe, delegate_args, {
    stdio: "inherit",
    env: { ...process.env, RESCRIPT_STDLIB: stdlibDir },
  });
} catch (e) {
  if (e.code === "ENOENT") {
    console.error(String(e));
  }
  process.exit(2);
}
