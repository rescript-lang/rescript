// @ts-check

import * as assert from "node:assert";
import { existsSync } from "node:fs";
import { setup } from "#dev/process";

const { execBuild } = setup("./a");

const output = await execBuild();
console.log(output);

assert.ok(
  existsSync("./node_modules/c/lib/js/tests/test.mjs"),
  "dev files of module 'c' were not built by 'a' even though 'c' is a transitive pinned dependency of 'a' through 'b'",
);
