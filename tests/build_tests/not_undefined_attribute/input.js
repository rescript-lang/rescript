// @ts-check

import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild, execClean } = setup(import.meta.dirname);

const out = await execBuild();
const stderr = normalizeNewlines(stripVTControlCharacters(out.stderr));

assert.ok(stderr.includes("demo.res:2:1-12"));
assert.ok(stderr.includes("@notUndefined can only be used on abstract types"));
assert.ok(stderr.includes("Failed to Compile"));
await execClean();
