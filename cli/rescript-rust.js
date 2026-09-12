#!/usr/bin/env node

// @ts-check

import { rescript_rust_exe } from "./common/bins.js";
import { runBuildSystem } from "./common/runBuildSystem.js";

if (rescript_rust_exe === undefined) {
  console.error(
    "The Rust build-system binary is not available for this platform.",
  );
  process.exit(1);
} else {
  runBuildSystem(rescript_rust_exe);
}
