// @ts-check

import assert from "node:assert";
import fs from "node:fs/promises";
import path from "node:path";
import { setup } from "#dev/process";

const { execBuildLegacy } = setup(import.meta.dirname);

await execBuildLegacy();

const o = await fs.readFile(path.join("src", "hello.res.js"), "ascii");
assert.ok(/HelloGen\.f/.test(o));
