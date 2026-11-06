// @ts-check

// The examples compiled with -bs-emit-ts. The generated src/*.res.ts are
// checked in, and so is tsc.txt: what tsc --strict reports for them against
// the runtime declarations in runtime-dts/.

import * as assert from "node:assert";
import * as child_process from "node:child_process";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);

await execClean();
await execBuildOrThrow();

const tsc = child_process.spawnSync(
  process.execPath,
  [
    path.join(import.meta.dirname, "../../../node_modules/typescript/bin/tsc"),
    "-p",
    ".",
    "--pretty",
    "false",
  ],
  { cwd: import.meta.dirname, encoding: "utf8" },
);
assert.equal(tsc.stderr, "");
await fs.writeFile(path.join(import.meta.dirname, "tsc.txt"), tsc.stdout);
