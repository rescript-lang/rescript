// @ts-check

import * as assert from "node:assert";
import * as os from "node:os";
import { setup } from "#dev/process";

const { execBuildLegacy } = setup(import.meta.dirname);

const out = await execBuildLegacy();
if (out.stdout !== "") {
  assert.fail(out.stdout);
} else {
  assert.equal(
    out.stderr,
    [
      'File "rescript.json", line 1',
      "Error: package weird not found or built",
      "- Did you install it?",
      "",
    ].join(os.EOL),
  );
}
