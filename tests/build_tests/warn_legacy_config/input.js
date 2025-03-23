// @ts-check

import * as assert from "node:assert";
import { setupWithUrl } from "#dev/process";

const { execBuild } = setupWithUrl(import.meta.url);

const output = await execBuild();
assert.match(
  output.stdout,
  /^Warning: bsconfig.json is deprecated. Migrate it to rescript.json/,
);
