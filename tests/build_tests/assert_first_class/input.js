// @ts-check

import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild, execClean } = setup(import.meta.dirname);

const out = await execBuild();
const stderr = normalizeNewlines(stripVTControlCharacters(out.stderr));

assert.notEqual(out.status, 0);
assert.match(stderr, /Main\.res:1:9-14/);
assert.match(
  stderr,
  /`assert` can only be used as a direct call like `assert\(condition\)`/,
);
assert.match(stderr, /It cannot be aliased or passed as a function/);

await execClean();
