// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
await execBuild();

const outputPath = path.join(import.meta.dirname, "src", "MainLayout.res.mjs");
const output = await fs.readFile(outputPath, "utf8");

assert.match(
  output,
  /let DynamicSidebar = await import\("\.\/Sidebar\.res\.mjs"\);/,
);
assert.match(output, /let dynamicProvider = DynamicSidebar\.Provider\.make;/);
assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscDynamicImportNestedJsx\.Provider,/,
);
assert.doesNotMatch(output, /let dynamicProvider = DynamicSidebar\.Sidebar\$Provider;/);
assert.doesNotMatch(output, /JsxRuntime\.jsx\(Sidebar\$RscDynamicImportNestedJsx\.Sidebar\$Provider,/);

await execClean();
