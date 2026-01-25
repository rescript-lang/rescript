// @ts-check

import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
const output = await execBuild();
const stderr = stripVTControlCharacters(output.stderr);

// verify the output is in reason syntax
const u = stderr.match(/=>/g);

const lines = stderr
  .split(/\r?\n/)
  .map(x => x.trim())
  .filter(Boolean);

let test = false;
for (let i = 0; i < lines.length; i++) {
  if (lines[i] === "We've found a bug for you!") {
    assert.match(lines[i + 1], /src[\\/]demo.res:1:21-23/);
    test = true;
  }
}
assert.ok(test);
assert.equal(u?.length, 2);
await execClean();
