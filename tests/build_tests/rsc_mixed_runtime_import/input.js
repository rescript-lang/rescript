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
  /import \* as Sidebar\$RscMixedRuntimeImport from "\.\/Sidebar\.res\.mjs";/,
);
assert.match(
  output,
  /import \* as Stdlib_OptionJs from "@rescript\/runtime\/lib\/es6\/Stdlib_Option\.js";/,
);
assert.match(
  output,
  /JsxRuntime\.jsx\(Sidebar\$RscMixedRuntimeImport\.Provider,/,
);
assert.doesNotMatch(output, /@rescript\/runtime\/lib\/es6\/Stdlib_Option\.mjs/);
assert.equal(
  output.match(/@rescript\/runtime\/lib\/es6\/Stdlib_Option\.js/g)?.length ?? 0,
  1,
);

await execClean();
