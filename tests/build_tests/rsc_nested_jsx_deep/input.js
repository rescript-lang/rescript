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
  /JsxRuntime\.jsx\(Sidebar\$RscNestedJsxDeep\.Sidebar\$Group,/,
);
assert.doesNotMatch(output, /\.Group\.make,/);
assert.match(sidebarOutput, /Sidebar\$Group\$jsx/);
assert.match(
  sidebarOutput,
  /export \{[\s\S]*Group,[\s\S]*Sidebar\$Group,[\s\S]*Sidebar\$Group\$jsx[\s\S]*\}/s,
);

const brandIcons = await import("./src/BrandIcons.res.js");
assert.deepStrictEqual(
  new Set(Object.keys(brandIcons)),
  new Set([
    "ReScript",
    "BrandIcons$ReScript",
    "BrandIcons$ReScript$jsx",
    "getIconForLanguageExtension",
  ]),
);

const multipleNested = await import("./src/MultipleNested.res.js");
assert.deepStrictEqual(
  new Set(Object.keys(multipleNested)),
  new Set([
    "Group",
    "MultipleNested$Group",
    "MultipleNested$Group$jsx",
    "Other",
    "MultipleNested$Other",
    "MultipleNested$Other$jsx",
  ]),
);

await execClean();
