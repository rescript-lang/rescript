// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { rescript } = setup(import.meta.dirname);

// Shows build output for `rescript build` command
let out = await rescript("build");
// Timing text only appears with TTY/progress output; plain output omits it.
assert.match(
  normalizeNewlines(out.stdout),
  /Parsed \d+ source files( in [0-9.]+s)?/,
);
assert.match(
  normalizeNewlines(out.stdout),
  /Compiled \d+ modules( in [0-9.]+s)?/,
);

// Shows build output for `rescript` command
out = await rescript("");
assert.match(
  normalizeNewlines(out.stdout),
  /Parsed \d+ source files( in [0-9.]+s)?/,
);
assert.match(
  normalizeNewlines(out.stdout),
  /Compiled \d+ modules( in [0-9.]+s)?/,
);

out = await rescript("build", ["-v"]);
assert.match(normalizeNewlines(out.stdout), /Created project context/);
