// @ts-check

import * as child_process from "node:child_process";
import * as fs from "node:fs";
import * as path from "node:path";

export const compilerRootDir = path.join(
  import.meta.dirname,
  "..",
  "..",
  "..",
);

// The playground-bundling root dir
export const playgroundDir = path.join(import.meta.dirname, "..");

// Final target output directory where all the cmijs will be stored
export const playgroundPackagesDir = path.join(playgroundDir, "packages");

/**
 * @param {string} cmd
 */
export function exec(cmd) {
  console.log(`>>>>>> running command: ${cmd}`);
  child_process.execSync(cmd, {
    cwd: playgroundDir,
    encoding: "utf8",
    stdio: "inherit",
  });
  console.log("<<<<<<");
}
