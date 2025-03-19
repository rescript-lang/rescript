#!/usr/bin/env node

// @ts-check

const child_process = require("node:child_process");
const path = require("node:path");

const platform = process.platform;
const ninjaDir = path.join(__dirname, "..", "ninja");
const buildCommand = "python configure.py --bootstrap --verbose";

if (platform === "win32") {
  // On Windows, the build uses the MSVC compiler which needs to be on the path.
  child_process.execSync(buildCommand, { cwd: ninjaDir });
} else {
  if (process.platform === "darwin") {
    process.env.CXXFLAGS = "-flto";
  }
  child_process.execSync(buildCommand, { stdio: [0, 1, 2], cwd: ninjaDir });
  child_process.execSync("strip ninja", { stdio: [0, 1, 2], cwd: ninjaDir });
}
