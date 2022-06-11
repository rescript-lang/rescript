var child_process = require("child_process");
var assert = require("assert");
var output = child_process.spawnSync(
  `../node_modules/.bin/rescript clean -with-deps && ../node_modules/.bin/rescript build`,
  {
    cwd: __dirname,
    encoding: "utf8",
    stdio: ["pipe", "pipe", "pipe"],
    shell: true,
  }
);
// console.log(output)
// var matches = output.match(/IGNORED/g)
// console.log(matches)

assert.equal(output.stderr.match(/IGNORED/g).length, 1);
assert.equal(output.stderr.indexOf("a-b.ml") > 0, true);
