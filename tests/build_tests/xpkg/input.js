// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuildLegacy } = await setup(import.meta.dirname);

const output = await execBuildLegacy(["-regen"]);
assert.match(output.stderr, /reserved package name/);
