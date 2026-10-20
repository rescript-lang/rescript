// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const out = await execBuild();
assert.equal(out.status, 2);
assert.match(out.stderr, /could not build package tree/i);
assert.match(out.stderr, /dependency 'weird'/i);
assert.match(out.stderr, /at path '.*weird_deps'/i);
await execClean();
