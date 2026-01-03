// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildLegacy, execCleanLegacy } = setup(import.meta.dirname);

await execCleanLegacy();
const output = await execBuildLegacy();

assert.match(output.stdout, /is dangling/);

const compilerLogFile = path.join("lib", "bs", ".compiler.log");
const compilerLog = await fs.readFile(compilerLogFile, "utf8");
assert.match(compilerLog, /is dangling/);
