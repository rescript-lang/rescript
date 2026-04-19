// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const output = await execBuild();
assert.equal(output.status, 0);
assert.match(output.stderr, /bsconfig\.json.*deprecated/i);
assert.match(output.stderr, /rename it to 'rescript\.json'/i);
await execClean();
