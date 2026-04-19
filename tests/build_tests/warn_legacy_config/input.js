// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const output = await execBuild();
assert.equal(output.status, 0);
assert.match(output.stderr, /uses deprecated config/i);
assert.match(output.stderr, /filename 'bsconfig\.json' .* rename to 'rescript\.json'/i);
await execClean();
