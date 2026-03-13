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

assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscComponentWithPropsNested\.Sidebar\$Provider,/,
);
assert.doesNotMatch(output, /\.Provider\.make,/);
assert.match(sidebarOutput, /Sidebar\$Provider\$jsx/);
assert.match(
  sidebarOutput,
  /export \{[\s\S]*Provider,[\s\S]*Sidebar\$Provider,[\s\S]*Sidebar\$Provider\$jsx[\s\S]*\}/s,
);

await execClean();
