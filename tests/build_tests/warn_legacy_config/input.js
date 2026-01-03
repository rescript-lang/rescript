// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuildLegacy } = setup(import.meta.dirname);

const output = await execBuildLegacy();
assert.match(
  output.stdout,
  /^Warning: bsconfig.json is deprecated. Migrate it to rescript.json/,
);
