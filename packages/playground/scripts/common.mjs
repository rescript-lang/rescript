// @ts-check

import * as child_process from "node:child_process";
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
 * @param {child_process.ExecSyncOptions} [opts={}]
 */
export function exec(cmd, opts = {}) {
  console.log(`>>>>>> running command: ${cmd}`);
  const result = child_process.execSync(cmd, {
    cwd: playgroundDir,
    stdio: "inherit",
    ...opts,
    encoding: "utf8",
  });
  console.log("<<<<<<");
  return result || "";
}
