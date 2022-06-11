var cp = require("child_process");
var assert = require("assert");

var out = cp.spawnSync(`../node_modules/.bin/rescript build`, {
  encoding: "utf-8",
  shell: true,
});
if (out.error) {
  throw out.error;
}
if (out.status !== 0) {
  assert.fail(out.stderr + "\n" + out.stdout);
}
