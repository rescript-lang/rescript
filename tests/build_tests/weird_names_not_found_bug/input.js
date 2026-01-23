import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";

const { execBuild, execClean } = await setup(import.meta.dirname);

const out = await execBuild();
const stderr = stripVTControlCharacters(out.stderr);

assert.ok(stderr.includes(`The module or file Demo can't be found.`));
await execClean();
