// @ts-check

import assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild } = setup(import.meta.dirname);

const out = await execBuild();
assert.match(
  out.stderr,
  /deprecated: Option "es6-global" is deprecated\. Use "esmodule" instead\./,
);
