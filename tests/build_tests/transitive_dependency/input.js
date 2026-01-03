// @ts-check

import * as assert from "node:assert";
import { existsSync } from "node:fs";
import { setup } from "#dev/process";

const { execBuildLegacy, execCleanLegacy } = setup("./a");
await execCleanLegacy();
const output = await execBuildLegacy();
console.log(output);

assert.ok(
  !existsSync("./node_modules/c/lib/es6/tests/test.res.js"),
  "dev files of module 'c' were built by 'a' even though 'c' is not a dependency of 'a'",
);
