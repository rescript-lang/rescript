// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild } = setup(import.meta.dirname);

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

await execBuild();
const content = await fs.readFile(
  path.join("lib", "bs", ".sourcedirs.json"),
  "utf-8",
);

assert.ok(JSON.parse(content).dirs.some(x => x.includes("📕annotation")));
