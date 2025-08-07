#!/usr/bin/env node

// @ts-check

import * as child_process from "node:child_process";
import { rescript_exe } from "./common/bins.js";
import { stdlibDir } from "./common/stdlib.js";

const args = process.argv.slice(2);

try {
  child_process.execFileSync(rescript_exe, args, {
    stdio: "inherit",
    env: { ...process.env, RESCRIPT_STDLIB: stdlibDir },
  });
} catch (err) {
  if (err.status !== undefined) {
    process.exit(err.status); // Pass through the exit code
  } else {
    process.exit(1); // Generic error
  }
}
