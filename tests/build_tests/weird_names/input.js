const cp = require("node:child_process");
const assert = require("node:assert");
const path = require("node:path");
const { rescript_exe } = require("#cli/bin_path");

const out = cp.spawnSync(rescript_exe, { encoding: "utf8" });

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
  const m = `./lib/js/src/${name}.js`;
  assert.deepEqual(require(m).a, 1);
}
