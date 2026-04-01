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
const plainAccessOutput = await fs.readFile(
  path.join(import.meta.dirname, "src", "PlainAccess.res.js"),
  "utf8",
);

assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscComponentWithPropsMembers\.Sidebar\$Provider,/,
);
assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscComponentWithPropsMembers\.Sidebar\$Inset,/,
);
assert.doesNotMatch(output, /\.Provider\.make,/);
assert.doesNotMatch(output, /\.Inset\.make,/);
assert.match(sidebarOutput, /Sidebar\$Provider\$jsx/);
assert.match(sidebarOutput, /Sidebar\$Inset\$jsx/);
assert.match(
  sidebarOutput,
  /export \{[\s\S]*Provider,[\s\S]*Inset,[\s\S]*Sidebar\$Provider,[\s\S]*Sidebar\$Inset,[\s\S]*\}/s,
);
assert.match(
  plainAccessOutput,
  /let provider = Sidebar\$RscComponentWithPropsMembers\.Provider\.make;/,
);
assert.match(
  plainAccessOutput,
  /let inset = Sidebar\$RscComponentWithPropsMembers\.Inset\.make;/,
);
assert.doesNotMatch(plainAccessOutput, /Sidebar\$Provider/);
assert.doesNotMatch(plainAccessOutput, /Sidebar\$Inset/);

await execClean();
