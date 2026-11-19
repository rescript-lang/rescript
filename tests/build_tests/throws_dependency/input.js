// @ts-check

// A module named only in a @throws is a dependency: the type checker looks
// the exception up, so the module is built first and the name resolves.

import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
const output = await execBuild();
const all = stripVTControlCharacters(output.stdout + output.stderr);

assert.equal(output.status, 0, all);
assert.doesNotMatch(all, /Warning number 113/);
await execClean();
