#!/usr/bin/env node

// @ts-check

// Verify that the compiler binaries in the platform npm package were
// produced by dune promotion from the current build (compiler/sync/dune is
// their only producer). Fails when a binary is missing - e.g. this
// platform's promotion rule did not fire - or when a stale binary from an
// earlier build is still in place.

import * as fs from "node:fs";
import * as path from "node:path";
import { binDir } from "#cli/bins";

const syncDir = path.join(
  import.meta.dirname,
  "..",
  "_build",
  "default",
  "compiler",
  "sync",
);

let ok = true;
for (const exe of ["bsc", "rescript-editor-analysis", "rescript-tools"]) {
  const promoted = path.join(binDir, `${exe}.exe`);
  const built = path.join(syncDir, `${exe}.exe`);
  if (
    !fs.existsSync(promoted) ||
    !fs.existsSync(built) ||
    !fs.readFileSync(promoted).equals(fs.readFileSync(built))
  ) {
    console.error(`Error: ${promoted} does not match ${built}.`);
    ok = false;
  } else if (process.platform !== "win32") {
    // Content being right is not enough: an archive round-trip can drop the
    // executable bit while preserving bytes.
    try {
      fs.accessSync(promoted, fs.constants.X_OK);
    } catch {
      console.error(`Error: ${promoted} is not executable.`);
      ok = false;
    }
  }
}

if (!ok) {
  console.error(
    "Dune promotion did not produce these binaries; check that this platform is covered by a rule in compiler/sync/dune.",
  );
  process.exit(1);
}

console.log("Compiler binaries in the platform package match the dune build.");
