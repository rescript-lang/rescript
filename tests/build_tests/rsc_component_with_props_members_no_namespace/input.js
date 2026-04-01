// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
await execBuild();

const output = await fs.readFile(
  path.join(import.meta.dirname, "src", "MainLayout.res.js"),
  "utf8",
);
const sidebarOutput = await fs.readFile(
  path.join(import.meta.dirname, "src", "Sidebar.res.js"),
  "utf8",
);

assert.match(output, /JsxRuntime\.jsx\(Sidebar\.Sidebar\$Provider,/);
assert.doesNotMatch(output, /Sidebar\.Provider\.make/);
assert.doesNotMatch(output, /Sidebar\.Sidebar\$Sidebar\$Provider/);
assert.match(sidebarOutput, /Sidebar\$Provider\$jsx/);
assert.match(
  sidebarOutput,
  /export \{[\s\S]*Provider,[\s\S]*Sidebar\$Provider,[\s\S]*Sidebar\$Provider\$jsx[\s\S]*\}/s,
);

await execClean();
