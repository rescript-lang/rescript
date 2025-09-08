// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild } = setup(import.meta.dirname);

const output = await execBuild(["-regen"]);
assert.match(output.stderr, /detected two module formats/);
