const cp = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

cp.execSync(rescript_exe, { cwd: __dirname, encoding: "utf8" });

const output = cp.execSync(
  `${rescript_exe} build -- -t commands src/hello.ast`,
  {
    cwd: __dirname,
    encoding: "utf8",
  },
);

assert.ok(
  /-ppx '.*\/test\.js -hello' -ppx '.*\/test\.js -heyy' -ppx .*test\.js/.test(
    output,
  ),
);
