// @ts-check

import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

await execBuild();

await fs.access(
  path.join(import.meta.dirname, "lib", "bs", "src", "📕annotation", "a.js"),
);
await execClean();
