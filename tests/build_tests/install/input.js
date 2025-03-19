const p = require("node:child_process");
const fs = require("node:fs");
const path = require("node:path");
const assert = require("node:assert");
const { rescript_exe } = require("#cli/bin_path");

p.spawnSync(rescript_exe, ["clean"], {
  encoding: "utf8",
  cwd: __dirname,
});
p.spawnSync(rescript_exe, ["build", "-install"], {
  encoding: "utf8",
  cwd: __dirname,
});

let fooExists = fs.existsSync(path.join(__dirname, "lib", "ocaml", "Foo.cmi"));
assert.ok(fooExists === false);

p.spawnSync(rescript_exe, {
  encoding: "utf8",
  cwd: __dirname,
});
p.spawnSync(rescript_exe, ["build", "-install"], {
  encoding: "utf8",
  cwd: __dirname,
});

fooExists = fs.existsSync(path.join(__dirname, "lib", "ocaml", "Foo.cmi"));
assert.ok(fooExists);
