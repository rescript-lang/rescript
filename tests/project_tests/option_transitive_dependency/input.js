// @ts-check

import * as assert from "node:assert/strict";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { pathToFileURL } from "node:url";
import { setup } from "#dev/process";

const root = import.meta.dirname;
const { bsc } = setup(root);
const outputDir = path.join(root, "lib");

/**
 * @param {string} directory
 * @param {string} source
 * @param {string[]} includes
 * @param {string[]} [extraFlags]
 */
async function compile(directory, source, includes, extraFlags = []) {
  const output = path.join(outputDir, directory);
  const stem = path.basename(source, path.extname(source));
  await fs.mkdir(output, { recursive: true });
  const result = await bsc([
    "-bs-project-root",
    root,
    "-bs-package-name",
    "option_transitive_dependency",
    "-bs-package-output",
    `esmodule:lib/${directory}:.mjs`,
    ...includes.flatMap(include => ["-I", path.join(outputDir, include)]),
    ...extraFlags,
    "-o",
    path.join(output, `${stem}.${source.endsWith(".resi") ? "cmi" : "cmj"}`),
    path.join(root, directory, source),
  ]);
  assert.equal(result.status, 0, `${source}: ${result.stdout}${result.stderr}`);
}

await fs.rm(outputDir, { recursive: true, force: true });

try {
  await compile("dep", "A.resi", ["dep"]);
  await compile("dep", "A.res", ["dep"], ["-bs-read-cmi"]);
  await compile("facade", "Facade.res", ["dep", "facade"]);
  // The consumer can load Facade, but not the dependency declaring its types.
  await compile("app", "Main.res", ["facade", "app"]);

  const main = await import(
    pathToFileURL(path.join(outputDir, "app", "Main.mjs")).href
  );
  const facade = await import(
    pathToFileURL(path.join(outputDir, "facade", "Facade.mjs")).href
  );
  assert.equal(main.variant, facade.variant);
  assert.deepEqual(main.record, { value: 42 });
  assert.equal(main.alias, 42);

  // Some(undefined) must remain distinct from None, including optional args.
  for (const field of [
    "abstractIsSome",
    "unboxedIsSome",
    "abstractRoundTrip",
    "unboxedRoundTrip",
    "optionalAbstractIsSome",
    "optionalUnboxedIsSome",
    "optionalAbstractRoundTrip",
    "optionalUnboxedRoundTrip",
  ]) {
    assert.equal(main[field], true, field);
  }
  assert.equal(main.omittedAbstract, undefined);
  assert.equal(main.omittedUnboxed, undefined);
} finally {
  await fs.rm(outputDir, { recursive: true, force: true });
}
