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
  /JsxRuntime\.jsx\(Sidebar\$RscComponentWithPropsMembers\.Provider,/,
);
assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscComponentWithPropsMembers\.Inset,/,
);
assert.doesNotMatch(output, /\.Provider\.make,/);
assert.doesNotMatch(output, /\.Inset\.make,/);
assert.match(sidebarOutput, /let Provider = Sidebar\$Provider;/);
assert.match(sidebarOutput, /let Inset = Sidebar\$Inset;/);
assert.match(
  sidebarOutput,
  /export \{[\s\S]*Provider,[\s\S]*Inset[\s\S]*\}/s,
);
assert.doesNotMatch(sidebarOutput, /Provider\.make = Provider;/);
assert.doesNotMatch(sidebarOutput, /Inset\.make = Inset;/);
assert.doesNotMatch(sidebarOutput, /Sidebar\$Provider\$jsx/);
assert.doesNotMatch(sidebarOutput, /Sidebar\$Inset\$jsx/);
assert.doesNotMatch(sidebarOutput, /export \{[\s\S]*Sidebar\$Provider[\s\S]*\}/s);
assert.doesNotMatch(sidebarOutput, /export \{[\s\S]*Sidebar\$Inset[\s\S]*\}/s);
assert.match(
  plainAccessOutput,
  /let provider = Sidebar\$RscComponentWithPropsMembers\.Provider;/,
);
assert.match(
  plainAccessOutput,
  /let inset = Sidebar\$RscComponentWithPropsMembers\.Inset;/,
);
assert.doesNotMatch(plainAccessOutput, /\.Provider\.make/);
assert.doesNotMatch(plainAccessOutput, /\.Inset\.make/);
assert.doesNotMatch(plainAccessOutput, /Sidebar\$Provider/);
assert.doesNotMatch(plainAccessOutput, /Sidebar\$Inset/);

await execClean();
