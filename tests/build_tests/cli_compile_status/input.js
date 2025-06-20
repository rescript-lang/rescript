// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { rescript } = setup(import.meta.dirname);

// Shows compile time for `rescript build` command
let out = await rescript("build");
assert.match(
  normalizeNewlines(out.stdout),
  />>>> Start compiling\nDependency Finished\n>>>> Finish compiling \d+ mseconds/,
);

// Shows compile time for `rescript` command
out = await rescript("build");
assert.match(
  normalizeNewlines(out.stdout),
  />>>> Start compiling\nDependency Finished\n>>>> Finish compiling \d+ mseconds/,
);

// Doesn't show compile time for `rescript build -verbose` command
// Because we can't be sure that -verbose is a valid argument
// And bsb won't fail with a usage message.
// It works this way not only for -verbose, but any other arg, including -h/--help/-help
out = await rescript("build", ["-verbose"]);

assert.match(
  normalizeNewlines(out.stdout),
  /Package stack: test {2}\nDependency Finished\n/,
);
assert.match(normalizeNewlines(out.stdout), /ninja.exe"? -C lib[\\/]bs ?\n/);
