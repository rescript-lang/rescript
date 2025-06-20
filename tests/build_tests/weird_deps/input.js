// @ts-check

import * as assert from "node:assert";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { execBuild } = setup(import.meta.dirname);

const out = await execBuild();
if (out.stdout !== "") {
  assert.fail(out.stdout);
} else {
  assert.equal(
    normalizeNewlines(out.stderr),
    [
      'File "rescript.json", line 1',
      "Error: package weird not found or built",
      "- Did you install it?",
      "",
    ].join("\n"),
  );
}
