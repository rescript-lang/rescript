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
const externalOutputPath = path.join(
  import.meta.dirname,
  "src",
  "MainLayoutExternal.res.js",
);
const externalOutput = await fs.readFile(externalOutputPath, "utf8");
const externalSidebarOutputPath = path.join(
  import.meta.dirname,
  "src",
  "SidebarExternal.res.js",
);
const externalSidebarOutput = await fs.readFile(
  externalSidebarOutputPath,
  "utf8",
);
const plainAccessOutputPath = path.join(
  import.meta.dirname,
  "src",
  "PlainAccess.res.js",
);
const plainAccessOutput = await fs.readFile(plainAccessOutputPath, "utf8");

assert.match(
  output,
  /import \* as Sidebar\$RscNestedJsxMembers from "\.\/Sidebar\.res\.js";/,
);
assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscNestedJsxMembers\.Sidebar\$Provider,/,
);
assert.match(
  externalOutput,
  /JsxRuntime\.jsx\(SidebarExternal\$RscNestedJsxMembers\.SidebarExternal\$Provider,/,
);
assert.doesNotMatch(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscNestedJsxMembers\.Sidebar\$RscNestedJsxMembers\$Provider,/,
);
assert.doesNotMatch(
  externalOutput,
  /JsxRuntime\.jsx\(SidebarExternal\$RscNestedJsxMembers\.Provider\.make,/,
);
assert.doesNotMatch(output, /\.Provider\.make,/);
assert.match(
  sidebarOutput,
  /export \{[\s\S]*Provider,[\s\S]*Inset,[\s\S]*Sidebar\$Provider,[\s\S]*Sidebar\$Inset,[\s\S]*\}/s,
);
assert.match(sidebarOutput, /Sidebar\$Provider\$jsx/);
assert.match(sidebarOutput, /Sidebar\$Inset\$jsx/);
assert.match(
  externalSidebarOutput,
  /export \{[\s\S]*Provider,[\s\S]*SidebarExternal\$Provider,[\s\S]*SidebarExternal\$Provider\$jsx[\s\S]*\}/s,
);
assert.match(
  plainAccessOutput,
  /let provider = Sidebar\$RscNestedJsxMembers\.Provider\.make;/,
);
assert.match(
  plainAccessOutput,
  /let callProvider = Sidebar\$RscNestedJsxMembers\.Provider\.make\(\{/,
);
assert.doesNotMatch(plainAccessOutput, /Sidebar\$Provider/);

await execClean();
