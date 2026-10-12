// @ts-check

import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild, execClean } = setup(import.meta.dirname);

const out = await execBuild();
const stderr = normalizeNewlines(stripVTControlCharacters(out.stderr));

assert.ok(stderr.includes("Main.res:3:3-14"));
assert.ok(
  stderr.includes(
    "This untagged variant definition is invalid: Case Bool can never match: both true and false are already cases of this variant.",
  ),
);
assert.ok(stderr.includes("Failed to Compile"));
await execClean();
