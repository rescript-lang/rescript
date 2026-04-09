// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
await execBuild();

const layoutOutput = await fs.readFile(
  path.join(import.meta.dirname, "src", "MainLayout.res.js"),
  "utf8",
);
const plainAccessOutput = await fs.readFile(
  path.join(import.meta.dirname, "src", "PlainAccess.res.js"),
  "utf8",
);

assert.match(
  layoutOutput,
  /JsxRuntime\.jsx\(Sidebar\$RscNestedJsxAliasChain\.Provider,/,
);
assert.doesNotMatch(layoutOutput, /\.Provider\.make,/);
assert.match(
  plainAccessOutput,
  /let provider = Sidebar\$RscNestedJsxAliasChain\.Provider;/,
);
assert.match(
  plainAccessOutput,
  /let callProvider = Sidebar\$RscNestedJsxAliasChain\.Provider\(\{/,
);
assert.doesNotMatch(plainAccessOutput, /\.Provider\.make/);
assert.doesNotMatch(plainAccessOutput, /Sidebar\$Provider/);

await execClean();
