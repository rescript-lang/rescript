// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
await execBuild();

const outputPath = path.join(import.meta.dirname, "src", "MainLayout.res.js");
const output = await fs.readFile(outputPath, "utf8");
const sidebarOutputPath = path.join(
  import.meta.dirname,
  "src",
  "Sidebar.res.js",
);
const sidebarOutput = await fs.readFile(sidebarOutputPath, "utf8");

assert.match(output, /JsxRuntime\.jsx\(Sidebar\.Provider,/);
assert.doesNotMatch(output, /Sidebar\.Provider\.make/);
assert.doesNotMatch(output, /Sidebar\.Sidebar\$Provider/);
assert.match(
  sidebarOutput,
  /let Provider = Sidebar\$Provider;/,
);
assert.match(sidebarOutput, /export \{[\s\S]*Provider[\s\S]*\}/s);
assert.doesNotMatch(sidebarOutput, /Provider\.make = Provider;/);
assert.doesNotMatch(sidebarOutput, /Sidebar\$Provider\$jsx/);
assert.doesNotMatch(sidebarOutput, /export \{[\s\S]*Sidebar\$Provider[\s\S]*\}/s);

await execClean();
