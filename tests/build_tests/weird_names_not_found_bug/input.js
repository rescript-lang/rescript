import * as assert from "node:assert";
import { setupWithUrl } from "#dev/process";

const { execBuild } = await setupWithUrl(import.meta.url);

const out = await execBuild();

if (out.stderr !== "") {
  assert.fail(out.stderr);
}

if (!out.stdout.includes(`The module or file Demo can't be found.`)) {
  assert.fail(out.stdout);
}
