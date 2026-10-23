// @ts-check

import assert from "node:assert/strict";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { bsc } = setup(import.meta.dirname);

const generatedDir = await fs.mkdtemp(
  path.join(import.meta.dirname, "generated-"),
);
const compile = file =>
  bsc(["-nostdlib", "-nopervasives", "-I", generatedDir, file], {
    cwd: generatedDir,
  });

try {
  await fs.writeFile(path.join(generatedDir, "A.res"), "let x = 1\n");
  await fs.writeFile(path.join(generatedDir, "B.res"), "let y = A.x\n");
  const compiled = await compile("A.res");
  assert.equal(compiled.status, 0, compiled.stderr);
  const cmi = path.join(generatedDir, "A.cmi");
  const contents = await fs.readFile(cmi);
  const magic = contents.subarray(0, 9).toString("latin1");
  assert.match(magic, /^ResCmi\d{3}$/);

  // Replaces the magic number of A.cmi and compiles a module that reads it.
  const readWith = async header => {
    await fs.writeFile(
      cmi,
      Buffer.concat([Buffer.from(header, "latin1"), contents.subarray(9)]),
    );
    const result = await compile("B.res");
    assert.notEqual(result.status, 0);
    return result.stderr;
  };

  const older = "is not a compiled interface for this version of ReScript";
  // The OCaml-style magic numbers of earlier compilers are reported as older.
  assert.match(await readWith("Caml1999I040"), /an older version of ReScript/);
  assert.match(await readWith("ResCmi000"), /an older version of ReScript/);
  assert.match(await readWith("ResCmi999"), /a newer version of ReScript/);
  const unrelated = await readWith("NotACmi!!");
  assert.match(unrelated, /is not a compiled interface/);
  assert.doesNotMatch(unrelated, new RegExp(older));
} finally {
  await fs.rm(generatedDir, { recursive: true, force: true });
}
