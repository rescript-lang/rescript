// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const out = await execBuild();
// A dependency that cannot be used is a build error, not a usage error.
assert.equal(out.status, 1);
assert.match(out.stderr, /\bweird\b/);
assert.match(out.stderr, /'.*weird_devdeps'/);
await execClean();
