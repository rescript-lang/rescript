import { writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { runDaemonTest } from "../helpers/test-context-v2.mjs";

// Tests the --warn-error CLI flag. This flag is passed through the daemon to
// bsc as -warn-error <value>, promoting specific warnings to errors.
// Warning 26 = unused variable (triggered by unused let bindings inside functions).

describe("build-warn-error", () => {
  it("build succeeds with unused binding when --warn-error is not set", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      const filePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await writeFile(
        filePath,
        'let greeting = () => {\n  let unused = 5\n  "hello"\n}\n',
      );

      const result = await cli.build();
      expect(result.status).toBe(0);
    }));

  it("build fails when --warn-error promotes warning to error", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      const filePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await writeFile(
        filePath,
        'let greeting = () => {\n  let unused = 5\n  "hello"\n}\n',
      );

      const result = await cli.build(["--warn-error", "+26"]);
      expect(result.status).not.toBe(0);
    }));
});
