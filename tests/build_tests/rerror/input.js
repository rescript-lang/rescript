// @ts-check
const child_process = require("node:child_process");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

child_process.spawnSync(`${rescript_exe} clean`, {
  cwd: __dirname,
  encoding: "utf8",
});

const o = child_process.spawnSync(rescript_exe, {
  cwd: __dirname,
  encoding: "utf8",
  shell: true,
});

// verify the output is in reason syntax
const u = o.stdout.match(/=>/g);

const lines = o.stdout
  .split(/\r?\n/)
  .map(x => x.trim())
  .filter(Boolean);

let test = false;
for (let i = 0; i < lines.length; i++) {
  if (lines[i] === "We've found a bug for you!") {
    console.log(`line ${i} found`);
    assert.ok(/src[\\/]demo.res:1:21-23/.test(lines[i + 1]));
    test = true;
  }
}
assert.ok(test);
assert.ok(u?.length === 2);
