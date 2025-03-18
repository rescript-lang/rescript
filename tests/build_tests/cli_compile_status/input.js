// @ts-check

const assert = require("node:assert");
const path = require("node:path");
const child_process = require("node:child_process");
const { normalizeNewlines } = require("../utils.js");

const rescriptPath = path.join(__dirname, "..", "..", "..", "cli", "rescript");

// Shows compile time for `rescript build` command
let out = child_process.spawnSync("node", [rescriptPath, "build"], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.match(
  normalizeNewlines(out.stdout),
  />>>> Start compiling\nDependency Finished\n>>>> Finish compiling \d+ mseconds/,
);

// Shows compile time for `rescript` command
out = child_process.spawnSync("node", [rescriptPath], {
  encoding: "utf8",
  cwd: __dirname,
});
assert.match(
  normalizeNewlines(out.stdout),
  />>>> Start compiling\nDependency Finished\n>>>> Finish compiling \d+ mseconds/,
);

// Doesn't show compile time for `rescript build -verbose` command
// Because we can't be sure that -verbose is a valid argument
// And bsb won't fail with a usage message.
// It works this way not only for -verbose, but any other arg, including -h/--help/-help
out = child_process.spawnSync("node", [rescriptPath, "build", "-verbose"], {
  encoding: "utf8",
  cwd: __dirname,
});

assert.match(
  normalizeNewlines(out.stdout),
  /Package stack: test {2}\nDependency Finished\n/,
);
assert.match(normalizeNewlines(out.stdout), /ninja.exe"? -C lib[\\/]bs ?\n/);
