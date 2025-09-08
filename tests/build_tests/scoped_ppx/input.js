// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";

const { execBuild } = setup(import.meta.dirname);

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

await execBuild();
const output = await execBuild(["--", "-t", "commands", "src/hello.ast"]);

assert.match(
  output.stdout,
  /-ppx '.*\/test\.js -hello' -ppx '.*\/test\.js -heyy' -ppx .*test\.js/,
);
