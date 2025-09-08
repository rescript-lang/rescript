#!/usr/bin/env node

// @ts-check

import { execSync } from "node:child_process";
import { ninjaDir } from "#dev/paths";

const platform = process.platform;
const buildCommand = "python3 configure.py --bootstrap --verbose";

if (platform === "win32") {
  // On Windows, the build uses the MSVC compiler which needs to be on the path.
  execSync(buildCommand, { cwd: ninjaDir });
} else {
  if (process.platform === "darwin") {
    process.env.CXXFLAGS = "-flto";
  }
  execSync(buildCommand, { stdio: [0, 1, 2], cwd: ninjaDir });
  execSync("strip ninja", { stdio: [0, 1, 2], cwd: ninjaDir });
}
