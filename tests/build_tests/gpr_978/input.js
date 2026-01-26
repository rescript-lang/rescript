// @ts-check

import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const output = await execBuild();
const stderr = stripVTControlCharacters(output.stderr);
assert.match(stderr, /M is exported twice/);
await execClean();
