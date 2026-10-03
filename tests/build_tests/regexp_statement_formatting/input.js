// @ts-check

import assert from "node:assert/strict";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { pathToFileURL } from "node:url";
import { setup } from "#dev/process";

const { bsc } = setup(import.meta.dirname);
const formatted = await bsc(["-format", "Main.res"]);
assert.equal(formatted.status, 0, formatted.stderr);

const generatedDir = await fs.mkdtemp(
  path.join(import.meta.dirname, "generated-"),
);
try {
  const source = path.join(generatedDir, "Main.res");
  await fs.writeFile(source, formatted.stdout);
  const reformatted = await bsc(["-format", source]);
  assert.equal(reformatted.status, 0, reformatted.stderr);
  assert.equal(reformatted.stdout, formatted.stdout);

  const compiled = await bsc([
    "-check-lam",
    "-bs-project-root",
    import.meta.dirname,
    "-bs-package-name",
    "regexp-statement-formatting",
    "-bs-package-output",
    `esmodule:${path.basename(generatedDir)}:.mjs`,
    "-o",
    path.join(generatedDir, "Main.cmj"),
    source,
  ]);
  assert.equal(compiled.status, 0, compiled.stderr);

  const output = await import(
    pathToFileURL(path.join(generatedDir, "Main.mjs")).href
  );
  assert.equal(output.afterLet("a"), true);
  assert.equal(output.afterLet("b"), false);
  assert.equal(output.afterExpression("a"), true);
  assert.equal(output.intermediate(), true);
  assert.equal(output.bare().test("a"), true);
  assert.equal(output.dotAndWhitespace(" a"), true);
  assert.equal(output.ternaryAndBinary("a"), 1);
  assert.equal(output.ternaryAndBinary("b"), 0);
  assert.equal(output.division(24), 4);
  assert.equal(output.floatDivision(24), 4);
} finally {
  await fs.rm(generatedDir, { recursive: true, force: true });
}
