// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { rescript_tools_exe } from "#cli/bins";
import { setup } from "#dev/process";

const { bsc, rescript, execBin } = setup(import.meta.dirname);

const { version } = JSON.parse(
  await fs.readFile(
    path.resolve(import.meta.dirname, "..", "..", "..", "..", "package.json"),
    "utf8",
  ),
);

// Every CLI reports the version of the rescript package.
assert.equal(
  (await rescript("--version")).stdout.trim(),
  `rescript ${version}`,
);
assert.equal((await bsc(["-v"])).stdout.trim(), `ReScript ${version}`);
assert.equal(
  (await execBin(rescript_tools_exe, ["--version"])).stdout.trim(),
  version,
);
assert.equal(
  (await execBin(rescript_tools_exe, ["reanalyze", "-version"])).stdout.trim(),
  `reanalyze version ${version}`,
);
