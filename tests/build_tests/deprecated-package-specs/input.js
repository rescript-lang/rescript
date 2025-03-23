// @ts-check

import assert from "node:assert";
import { setupWithUrl } from "#dev/process";

const { execBuild } = setupWithUrl(import.meta.url);

const out = await execBuild();
assert.match(
  out.stderr,
  /deprecated: Option "es6-global" is deprecated\. Use "esmodule" instead\./,
);
