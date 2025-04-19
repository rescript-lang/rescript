// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
const output = await execBuild([]);

// verify the output is in reason syntax
const u = output.stdout.match(/=>/g);

const lines = output.stdout
  .split(/\r?\n/)
  .map(x => x.trim())
  .filter(Boolean);

let test = false;
for (let i = 0; i < lines.length; i++) {
  if (lines[i] === "We've found a bug for you!") {
    console.log(`line ${i} found`);
    assert.match(lines[i + 1], /src[\\/]demo.res:1:21-23/);
    test = true;
  }
}
assert.ok(test);
assert.equal(u?.length, 2);
