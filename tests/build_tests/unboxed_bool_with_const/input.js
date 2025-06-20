// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild } = setup(import.meta.dirname);

const out = await execBuild();

assert.equal(
  normalizeNewlines(out.stdout.slice(out.stdout.indexOf("Main.res:3:3-14"))),
  `Main.res:3:3-14

  1 │ @unboxed
  2 │ type t<'a> =
  3 │   | Bool(bool)
  4 │   | @as(false) False
  5 │   | @as(true) True

  This untagged variant definition is invalid: At most one case can be a boolean type.

FAILED: cannot make progress due to previous errors.
`,
);
