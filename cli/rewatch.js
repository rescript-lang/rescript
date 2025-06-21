#!/usr/bin/env node

// @ts-check

import * as child_process from "node:child_process";
import { rewatch_exe, bsc_exe } from "./common/bins.js";

const args = process.argv.slice(2);

const firstPositionalArgIndex = args.findIndex((arg) => !arg.startsWith("-"));

try {
  if (firstPositionalArgIndex !== -1) {
    const subcommand = args[firstPositionalArgIndex];
    const subcommandArgs = args.slice(firstPositionalArgIndex + 1);

    if (
      subcommand === "build" ||
      subcommand === "watch" ||
      subcommand === "clean" ||
      subcommand === "compiler-args"
    ) {
      child_process.execFileSync(
        rewatch_exe,
        [...subcommandArgs, "--bsc-path", bsc_exe],
        {
          stdio: "inherit",
        }
      );
    } else {
      child_process.execFileSync(rewatch_exe, [...args], {
        stdio: "inherit",
      });
    }
  }
} catch (err) {
  if (err.status !== undefined) {
    process.exit(err.status); // Pass through the exit code
  } else {
    process.exit(1); // Generic error
  }
}
