// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

await execClean();
const buildResult = await execBuild();

if (buildResult.status !== 0) {
  assert.fail(buildResult.stdout + buildResult.stderr);
}

const localOnlyOutput = await fs.readFile(
  path.join(import.meta.dirname, "src", "LocalOnly.res.js"),
  "utf8",
);
const consumerOutput = await fs.readFile(
  path.join(import.meta.dirname, "src", "Consumer.res.js"),
  "utf8",
);

assert.match(localOnlyOutput, /JsxRuntime\.jsx\(LocalOnly\$Hidden,/);
assert.doesNotMatch(localOnlyOutput, /export \{[\s\S]*Hidden[\s\S]*\}/s);
assert.doesNotMatch(localOnlyOutput, /LocalOnly\$Hidden\$jsx/);
assert.match(consumerOutput, /JsxRuntime\.jsx\(LocalOnly\.make,/);

await execClean();
