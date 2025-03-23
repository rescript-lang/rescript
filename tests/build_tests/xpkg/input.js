// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild } = await setup(import.meta.dirname);

const output = await execBuild(["-regen"]);
assert.match(output.stderr, /reserved package name/);
