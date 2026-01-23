// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const output = await execBuild();
assert.notEqual(output.status, 0);
assert.match(output.stderr, /could not read rescript\.json/i);
await execClean();
