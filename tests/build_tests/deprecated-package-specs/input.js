// @ts-check

import assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild, execClean } = setup(import.meta.dirname);

const { stdout } = await execBuild();

assert.match(
  stdout,
  /deprecated: Option "es6-global" is deprecated\. Use "esmodule" instead\./,
);

await execClean();
