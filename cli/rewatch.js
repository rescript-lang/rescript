#!/usr/bin/env node

// @ts-check

import * as child_process from "node:child_process";
import { rewatch_exe, bsc_exe } from "./common/bins.js";

const args = process.argv.slice(2);

try {
  child_process.execFileSync(rewatch_exe, [...args, "--bsc-path", bsc_exe], {
    stdio: "inherit",
  });
} catch (err) {
  if (err.status !== undefined) {
    process.exit(err.status); // Pass through the exit code
  } else {
    process.exit(1); // Generic error
  }
}
