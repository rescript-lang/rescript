// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild } = setup(import.meta.dirname);

const out = await execBuild();

assert.equal(
  normalizeNewlines(out.stdout.slice(out.stdout.indexOf("input.res:2:1-12"))),
  `input.res:2:1-12

  1 │ @notUndefined
  2 │ type t = int
  3 │ 

  @notUndefined can only be used on abstract types

FAILED: cannot make progress due to previous errors.
`,
);
