// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setupWithUrl } from "#dev/process";

const { execBuild } = setupWithUrl(import.meta.url);

const output = await execBuild();
assert.match(output.stdout, /M is exported twice/);

const compilerLogFile = path.join("lib", "bs", ".compiler.log");
const compilerLog = await fs.readFile(compilerLogFile, "utf8");
assert.match(compilerLog, /M is exported twice/);
