//@ts-check
var cp = require("child_process");
var path = require("path");
var assert = require("assert");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
var output = cp.execSync(`${rescript_exe} build -- -t targets`, {
  encoding: "utf8",
  cwd: __dirname
});

var cmjTargets = output
  .split("\n")
  .filter(Boolean)
  .map(x => x.split(":")[0])
  .filter(x => x.endsWith(".cmj"))
  .map(x => path.basename(x))
  .sort();
cmjTargets.sort()
var expected = ["demo.cmj", "hello00.cmj","hexll..cmj",'hexll.cmj']
expected.sort()
assert.deepEqual(cmjTargets, expected);


