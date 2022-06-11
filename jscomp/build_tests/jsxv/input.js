//@ts-check

var assert = require("assert");

var p = require("child_process");

var output = p.spawnSync(`../node_modules/.bin/rescript`, {
  cwd: __dirname,
  encoding: "utf8",
  shell: true,
});

assert.ok(output.stderr.match(/File ".*bsconfig.json", line 10/));
