var r = require("../../../vendor/rollup.js");
var path = require("path");
var assert = require("assert");
var p = require("child_process");
try {
  p.execSync(`../node_modules/.bin/rescript build`, {
    cwd: __dirname,
    encoding: "utf8",
    stdio: [0, 1, 2],
  });
  r.rollup({
    input: path.join(__dirname, "yy.js"),
  })
    .then((bundle) => {
      return bundle.generate({
        format: "iife",
        name: "X",
      });
    })
    .then((output) => {
      // console.log(output.code)
      assert.ok(output.code.length < 1000, "bundled success");
      p.execSync(`../node_modules/.bin/rescript clean -with-deps`);
    });
} finally {
}
