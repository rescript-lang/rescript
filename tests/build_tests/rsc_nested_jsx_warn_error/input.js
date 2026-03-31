// @ts-check

import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
const result = await execBuild();

if (result.status !== 0) {
  assert.fail(result.stdout + result.stderr);
}

const stderr = stripVTControlCharacters(result.stderr);
assert.doesNotMatch(stderr, /Warning number 32/);

await execClean();
