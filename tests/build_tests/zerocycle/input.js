// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild } = setup(import.meta.dirname);
const output = await execBuild();
assert.ok(output.status === 0);
