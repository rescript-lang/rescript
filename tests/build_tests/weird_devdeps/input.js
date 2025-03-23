// @ts-check

import * as assert from "node:assert";
import * as os from "node:os";
import { setupWithUrl } from "#dev/process";

const { execBuild } = setupWithUrl(import.meta.url);

const out = await execBuild();
if (out.stdout !== "") {
  assert.fail(out.stdout);
} else {
  assert.equal(
    out.stderr,
    [
      'File "bsconfig.json", line 1',
      "Error: package weird not found or built",
      "- Did you install it?",
      "",
    ].join(os.EOL),
  );
}
