// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const out = await execBuild();
assert.notEqual(out.status, 0);
assert.match(out.stderr, /could not build package tree/i);
assert.match(out.stderr, /dependency 'weird'/i);
await execClean();
