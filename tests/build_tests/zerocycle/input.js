// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuildLegacy } = setup(import.meta.dirname);
const output = await execBuildLegacy();
assert.ok(output.status === 0);
