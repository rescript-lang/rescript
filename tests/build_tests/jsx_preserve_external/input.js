// @ts-check

import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);

await execClean();
await execBuildOrThrow();

const output = readFileSync(new URL("src/Test.jsx", import.meta.url), "utf8");
assert.match(output, /import \* as SomeLib from "some-lib"/);
assert.match(output, /<SomeLib\.Head>\s*\{<div\s*\/>\}\s*<\/SomeLib\.Head>/);
assert.doesNotMatch(output, /=>/);
