import { describe, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("watch", () => {
  it("watches for file changes and rebuilds", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      const watch = cli.spawnWatch();
      await watch.waitForOutput(/Finished initial compilation/i, 15000);

      await writeFileInSandbox(
        "packages/library/src/Library.res",
        'let greeting = "modified"\n',
      );

      await watch.waitForOutput(/Finished.*compilation/i, 10000);
    }));
});
