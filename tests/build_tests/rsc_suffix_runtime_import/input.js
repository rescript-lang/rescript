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
  /import \* as Stdlib_Option from "@rescript\/runtime\/lib\/es6\/Stdlib_Option\.mjs";/,
);
assert.match(
  output,
  /import \* as Sidebar\$RscSuffixRuntimeImport from "\.\/Sidebar\.res\.mjs";/,
);
assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscSuffixRuntimeImport\.Provider,/,
);
assert.equal(output.match(/Stdlib_Option\.(js|mjs)/g)?.length ?? 0, 1);
assert.doesNotMatch(
  output,
  /@rescript\/runtime\/lib\/es6\/Stdlib_Option\.(js|mjs)";[\s\S]*@rescript\/runtime\/lib\/es6\/Stdlib_Option\.(js|mjs)";/s,
);

await execClean();
