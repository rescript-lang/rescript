// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild } = setup(import.meta.dirname);

await execBuild();

const content = await fs.readFile(path.join("src", "demo.js"), "utf8");

assert.equal(content.match(/A00_a1_main/g)?.length, 3);
assert.equal(content.match(/B00_b1_main/g)?.length, 3);
assert.equal(content.match(/A0_main/g)?.length, 2);
assert.equal(content.match(/a0_main/g)?.length, 1);
assert.equal(content.match(/B0_main/g)?.length, 2);
assert.equal(content.match(/b0_main/g)?.length, 1);

const mod = await import("./src/demo.js");
assert.equal(mod.v, 4, "nested");
