// @ts-check

import * as assert from "node:assert";
import * as path from "node:path";
import { setupWithUrl } from "#dev/process";

const { execBuild } = setupWithUrl(import.meta.url);

const out = await execBuild();

if (out.stderr !== "") {
  assert.fail(out.stderr);
}

const files = [
  "_app.res",
  "[...params_max_3].res",
  "[...params].res",
  "[[...params]].res",
  "[slug_or_ID].res",
  "404.res",
  "utils.test.res",
];

for (const f of files) {
  const { name } = path.parse(f);
  const mod = await import(`./lib/es6/src/${name}.js`);
  assert.deepEqual(mod.a, 1);
}
