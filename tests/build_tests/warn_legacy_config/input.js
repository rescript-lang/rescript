// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const output = await execBuild();
assert.notEqual(output.status, 0);
assert.match(output.stderr, /no package\.json or rescript\.json file/i);
await execClean();
