import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import {
  bumpMagicNumbers,
  checkMagicNumbers,
  configPath,
  incrementMagicNumber,
  previousReleaseTag,
  readMagicNumber,
  releaseMagicNumbers,
} from "./magicNumbers.js";

const config = (cmi, cmt, ast) => `let cmi_magic_number = "ResCmi${cmi}"

and ast_impl_magic_number = "ResImpl${ast}"

and ast_intf_magic_number = "ResIntf${ast}"

and ast0_impl_magic_number = "Caml1999M022"

and cmt_magic_number = "ResCmt${cmt}"
`;

test("increments the trailing counter and keeps its width", () => {
  assert.equal(incrementMagicNumber("ResCmi040"), "ResCmi041");
  assert.equal(incrementMagicNumber("ResCmi099"), "ResCmi100");
  assert.equal(incrementMagicNumber("ResImpl01317"), "ResImpl01318");
  assert.throws(() => incrementMagicNumber("ResCmi999"), /out of digits/);
  assert.throws(() => incrementMagicNumber("ResCmi"), /trailing counter/);
});

test("reads magic numbers by name", () => {
  const current = config("040", "048", "01317");
  assert.equal(readMagicNumber(current, "cmi_magic_number"), "ResCmi040");
  assert.equal(
    readMagicNumber(current, "ast_impl_magic_number"),
    "ResImpl01317",
  );
  assert.equal(readMagicNumber(current, "missing_magic_number"), null);
});

test("bumps the magic numbers of the previous release when the formats changed", () => {
  const previous = config("040", "048", "01317");
  assert.equal(bumpMagicNumbers(previous, previous, false), previous);
  assert.equal(
    bumpMagicNumbers(previous, previous, true),
    config("041", "049", "01318"),
  );
  // Leaves the frozen Parsetree0 numbers alone.
  assert.ok(
    bumpMagicNumbers(previous, previous, true).includes('"Caml1999M022"'),
  );
  // A number that differs from the previous release was already bumped.
  const bumped = config("041", "048", "01317");
  assert.equal(
    bumpMagicNumbers(bumped, previous, true),
    config("041", "049", "01318"),
  );
  assert.equal(
    bumpMagicNumbers(config("041", "049", "01318"), previous, true),
    config("041", "049", "01318"),
  );
  // So is every number when the previous release had other ones or no
  // config.ml at all.
  const legacy = previous.replace('"ResCmi040"', '"Caml1999I040"');
  assert.equal(
    bumpMagicNumbers(previous, legacy, true),
    config("040", "049", "01318"),
  );
  assert.equal(bumpMagicNumbers(previous, null, true), previous);
  assert.throws(
    () =>
      bumpMagicNumbers(
        previous.replace("cmt_magic_number", "other"),
        previous,
        true,
      ),
    /cmt_magic_number not found/,
  );
});

test("bumps the magic numbers once per release when format sources changed", async t => {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "rescript-magic-"));
  t.after(() => fs.rm(root, { recursive: true, force: true }));
  const git = (...args) =>
    execFileSync("git", args, {
      cwd: root,
      env: {
        ...process.env,
        GIT_AUTHOR_NAME: "a",
        GIT_AUTHOR_EMAIL: "a@example.com",
        GIT_COMMITTER_NAME: "a",
        GIT_COMMITTER_EMAIL: "a@example.com",
      },
    });
  const write = async (file, text) => {
    await fs.mkdir(path.dirname(path.join(root, file)), { recursive: true });
    await fs.writeFile(path.join(root, file), text);
  };
  const commit = async (file, text) => {
    await write(file, text);
    git("add", ".");
    git("commit", "-q", "-m", file);
  };
  git("init", "-q");
  await commit(configPath, config("040", "048", "01317"));
  assert.throws(() => previousReleaseTag(root, "1.1.0"), /git fetch --tags/);
  await commit("compiler/ml/types.ml", "type t = A\n");
  git("tag", "v1.0.0");

  assert.equal(await releaseMagicNumbers(root, "1.1.0"), null);
  await commit("compiler/syntax/printer.ml", "let print = ()\n");
  assert.equal(await releaseMagicNumbers(root, "1.1.0"), null);
  // Only committed changes count.
  await write("compiler/ml/types.ml", "type t = A | B\n");
  assert.equal(await releaseMagicNumbers(root, "1.1.0"), null);
  await commit("compiler/ml/types.ml", "type t = A | B\n");
  const bumped = await releaseMagicNumbers(root, "1.1.0");
  assert.equal(bumped, config("041", "049", "01318"));

  // A later run for the same release keeps the bumped numbers, also when the
  // tag of the version being released already exists.
  await commit(configPath, bumped);
  git("tag", "v1.1.0");
  assert.equal(previousReleaseTag(root, "1.1.0"), "v1.0.0");
  await commit("compiler/ext/ident.ml", "type t = string\n");
  assert.equal(await releaseMagicNumbers(root, "1.1.0"), null);
  // The next release compares with the one before it.
  assert.equal(
    await releaseMagicNumbers(root, "1.2.0"),
    config("042", "050", "01319"),
  );
});

test("rejects pull requests that bump a magic number", () => {
  const base = config("040", "048", "01317");
  checkMagicNumbers(base, base, []);
  checkMagicNumbers(null, base, []);
  assert.throws(
    () => checkMagicNumbers(base, config("040", "049", "01317"), []),
    /don't bump cmt_magic_number;/,
  );
  assert.throws(
    () => checkMagicNumbers(base, config("041", "048", "01318"), []),
    /cmi_magic_number, ast_impl_magic_number, ast_intf_magic_number/,
  );
  checkMagicNumbers(base, config("041", "049", "01318"), ["changelog:release"]);
  // Switching to another prefix is no bump.
  checkMagicNumbers(base.replace('"ResCmi040"', '"Caml1999I040"'), base, [
    "changelog:skip",
  ]);
});
