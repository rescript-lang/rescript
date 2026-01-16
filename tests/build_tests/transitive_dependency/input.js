// @ts-check

import * as assert from "node:assert";
import { existsSync } from "node:fs";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(path.join(import.meta.dirname, "a"));
await execClean();
await execBuild();

assert.ok(
  !existsSync(
    path.join(
      import.meta.dirname,
      "a",
      "node_modules",
      "c",
      "lib",
      "es6",
      "tests",
      "test.res.js",
    ),
  ),
  "dev files of module 'c' were built by 'a' even though 'c' is not a dependency of 'a'",
);
