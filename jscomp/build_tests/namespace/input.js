var child_process = require("child_process");
var fs = require("fs");
var path = require("path");
var rescript_exe = require("../../../scripts/bin_path").rescript_exe;
child_process.execSync(`${rescript_exe} clean -with-deps && ${rescript_exe} build`, {
  cwd: __dirname,
  stdio: [0, 1, 2],
});

var x = require("./src/demo.bs.js");
var assert = require("assert");
var demo_bs_js = fs.readFileSync(
  path.join(__dirname, "src", "demo.bs.js"),
  "utf8"
);
assert.ok(demo_bs_js.includes("liba/src/demo.bs.js"));
assert.equal(x.v, 3);

var merlin = fs.readFileSync(path.join(__dirname, ".merlin"), "utf8");
var warn_flag = "-40+6+7"; // Note it is additive now
assert.ok(merlin.includes("-open"));
assert.ok(merlin.includes(warn_flag));
assert.ok(merlin.includes("emptydir") !== true);

var testDepsNoWarning = "-w a";
function hasWarnError(file) {
  return fs.readFileSync(file, "utf8").includes(testDepsNoWarning);
}

var content =
  fs.readFileSync(path.join(__dirname, "lib", "bs", "build.ninja")) + "";
assert.ok(content.includes("emptydir") !== true);
assert.ok(!content.includes(testDepsNoWarning));
assert.ok(content.includes(warn_flag));
assert.ok(
  hasWarnError(
    path.join(__dirname, "node_modules", "liba", "lib", "bs", "build.ninja")
  )
);
assert.ok(
  hasWarnError(
    path.join(__dirname, "node_modules", "libb", "lib", "bs", "build.ninja")
  )
);
assert.ok(content.includes(".bs.js"));
