// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
await execBuild();

const outputPath = path.join(
  import.meta.dirname,
  "src",
  "MainLayout.custom.mjs",
);
const output = await fs.readFile(outputPath, "utf8");

assert.match(
  output,
  /let DynamicSidebar = await import\("\.\/Sidebar\.custom\.mjs"\);/,
);
assert.match(
  output,
  /let dynamicProvider = DynamicSidebar\.Sidebar\$Provider;/,
);
assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscDynamicImportNestedJsx\.Sidebar\$Provider,/,
);
assert.doesNotMatch(
  output,
  /let dynamicProvider = DynamicSidebar\.Provider\.make;/,
);
assert.doesNotMatch(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscDynamicImportNestedJsx\.Provider,/,
);

await execClean();
