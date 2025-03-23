// @ts-check

import * as assert from "node:assert";
import { setupWithUrl } from "#dev/process";

const { execBuild } = setupWithUrl(import.meta.url);
const output = await execBuild();
assert.ok(output.status === 0);
