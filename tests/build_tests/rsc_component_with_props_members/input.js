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
  /JsxRuntime\.jsx\(Sidebar\$RscComponentWithPropsMembers\.Sidebar\$Provider\$make,/,
);
assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscComponentWithPropsMembers\.Sidebar\$Inset\$make,/,
);
assert.doesNotMatch(output, /\.Provider\.make,/);
assert.doesNotMatch(output, /\.Inset\.make,/);
assert.match(
  sidebarOutput,
  /let Provider = \{[\s\S]*make: Sidebar\$Provider\$make[\s\S]*\};/s,
);
assert.match(
  sidebarOutput,
  /let Inset = \{[\s\S]*make: Sidebar\$Inset\$make[\s\S]*\};/s,
);
assert.match(
  sidebarOutput,
  /export \{[\s\S]*Sidebar\$Provider\$make,[\s\S]*Sidebar\$Inset\$make[\s\S]*\}/s,
);
assert.doesNotMatch(sidebarOutput, /Provider\.make = Provider;/);
assert.doesNotMatch(sidebarOutput, /Inset\.make = Inset;/);
assert.doesNotMatch(sidebarOutput, /Sidebar\$Provider\$jsx/);
assert.doesNotMatch(sidebarOutput, /Sidebar\$Inset\$jsx/);
assert.match(
  plainAccessOutput,
  /let provider = Sidebar\$RscComponentWithPropsMembers\.Sidebar\$Provider\$make;/,
);
assert.match(
  plainAccessOutput,
  /let inset = Sidebar\$RscComponentWithPropsMembers\.Sidebar\$Inset\$make;/,
);
assert.doesNotMatch(
  plainAccessOutput,
  /Sidebar\$RscComponentWithPropsMembers\.Provider\.make/,
);
assert.doesNotMatch(
  plainAccessOutput,
  /Sidebar\$RscComponentWithPropsMembers\.Inset\.make/,
);

await execClean();
