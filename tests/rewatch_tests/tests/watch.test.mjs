import { describe, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("watch", () => {
  it("watches for file changes and rebuilds", () =>
    runRewatchTest(async ({ cli, writeFile }) => {
      // Start watch mode
      const watch = cli.spawnWatch();

      // Wait for initial build to complete
      await watch.waitForOutput(/Finished initial compilation/i, 15000);

      // Modify a source file to trigger rebuild
      await writeFile(
        "packages/library/src/Library.res",
        'let greeting = "modified"\n',
      );

      // Wait for rebuild to complete
      await watch.waitForOutput(/Finished.*compilation/i, 10000);
    }));
});
