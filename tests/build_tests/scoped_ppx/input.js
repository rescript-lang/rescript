// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuildLegacy } = setup(import.meta.dirname);

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

await execBuildLegacy();
const output = await execBuildLegacy(["--", "-t", "commands", "src/hello.ast"]);

assert.match(
  output.stdout,
  /-ppx '.*\/test\.js -hello' -ppx '.*\/test\.js -heyy' -ppx .*test\.js/,
);
