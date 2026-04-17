// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs";
import * as path from "node:path";
import { setup } from "#dev/process";

const { bsc, execBuild, execClean } = setup(import.meta.dirname);

const lockFile = path.join(import.meta.dirname, "lib", "rescript.lock");
const sourceFile = path.join(import.meta.dirname, "src", "demo.res");
const outputFile = path.join(import.meta.dirname, "src", "demo.mjs");

try {
  const lambdaOut = await bsc(["-drawlambda", sourceFile], {
    throwOnFail: false,
  });
  const lambdaText = lambdaOut.stdout + lambdaOut.stderr;

  assert.equal(lambdaOut.status, 0, lambdaText);
  assert.match(lambdaText, /\bbreak\b/);
  assert.match(lambdaText, /\bcontinue\b/);

  fs.writeFileSync(lockFile, "39409");
  const buildOut = await execBuild();
  assert.equal(buildOut.status, 0, buildOut.stdout + buildOut.stderr);

  const js = fs.readFileSync(outputFile, "utf-8");
  assert.match(js, /\bbreak;/);
  assert.match(js, /\bcontinue;/);
  assert.match(js, /loop_\d+:\s*while/);
  assert.match(js, /break loop_\d+;/);
  assert.match(js, /continue loop_\d+;/);
} finally {
  await execClean();
}
