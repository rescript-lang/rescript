// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";

const { execBuild, execBuildOrThrow, execClean } = setup(import.meta.dirname);

await execClean();
await execBuildOrThrow();

const cart = await import(new URL("./src/Cart.res.mjs", import.meta.url).href);
assert.equal(cart.total, 42);
assert.equal(cart.viaAlias, 1);
assert.equal(cart.viaOpen, 3);
assert.equal(cart.shopName, "shop");

const shadow = await import(
  new URL("./src/Shadow.res.mjs", import.meta.url).href
);
assert.equal(shadow.label, "local");

// Errors name the modules of the package as found through the namespace
const typo = path.join(import.meta.dirname, "src", "Typo.res");
const typeError = path.join(import.meta.dirname, "src", "TypeError.res");
await fs.writeFile(typo, "let x = Prise.make(1)\n");
await fs.writeFile(typeError, "let x: string = Price.make(1)\n");
try {
  const output = await execBuild();
  const stderr = stripVTControlCharacters(output.stderr);
  assert.match(stderr, /The module or file Prise can't be found\./);
  // Candidates come in the order of the namespace's modules
  assert.match(stderr, /Hint: Did you mean Prize, Prime, Pride or Price\?/);
  assert.match(stderr, /This has type: Shop\.Price\.t\r?\n/);
} finally {
  await fs.rm(typo, { force: true });
  await fs.rm(typeError, { force: true });
}

await execClean();
