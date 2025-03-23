// @ts-check

import assert from "node:assert";
import fs from "node:fs/promises";
import path from "node:path";
import { setupWithUrl } from "#dev/process";

const { execBuild } = setupWithUrl(import.meta.url);

await execBuild();

const o = await fs.readFile(path.join("src", "hello.res.js"), "ascii");
assert.ok(/HelloGen\.f/.test(o));
