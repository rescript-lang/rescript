import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("lock", () => {
  it("prevents concurrent builds while watch is running", () =>
    runRewatchTest(async ({ cli }) => {
      const watch = cli.spawnWatch();
      await watch.waitForOutput(/Finished initial compilation/i, 15000);

      // Attempt a build while watcher holds the lock
      const result = await cli.build();

      expect(result.status).not.toBe(0);
      expect(result.stderr).toContain("A ReScript build is already running");
    }));
});
