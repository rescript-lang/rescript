import { rename, rm } from "node:fs/promises";
import { join } from "node:path";
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context-v2.mjs";

// Tests file rename scenarios where a module has both .res and .resi files.
// When only the .res file is renamed but the .resi is left behind, the daemon
// must handle this "orphaned interface" case gracefully without crashing.
//
// Fixture: ModuleWithInterface.res + ModuleWithInterface.resi in packages/with-interface/src

describe("watch-file-rename-with-interface", () => {
  it("clean + build handles orphaned .resi when .res is renamed", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      // Initial build should succeed
      await build(sandbox);

      // Rename only the .res file, leaving the .resi orphaned
      const mainSrc = join(sandbox, "packages", "with-interface", "src");
      await rename(
        join(mainSrc, "ModuleWithInterface.res"),
        join(mainSrc, "ModuleWithInterface2.res"),
      );

      // Clean to invalidate cached state, then rebuild
      await clean(sandbox);
      await build(sandbox);
    }));

  it("watch mode detects orphaned .resi when .res is renamed", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      // Start watch (waits for initial build)
      const watchHandle = await watch(sandbox);

      try {
        // Rename only the .res file while watch is running
        const mainSrc = join(sandbox, "packages", "with-interface", "src");
        await rename(
          join(mainSrc, "ModuleWithInterface.res"),
          join(mainSrc, "ModuleWithInterface2.res"),
        );

        // Wait for rebuild triggered by rename
        await watchHandle.waitForBuild();
      } finally {
        watchHandle.stop();
      }
    }));

  it("watch mode recovers when both .res and .resi are renamed together", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      // Start watch (waits for initial build)
      const watchHandle = await watch(sandbox);

      try {
        const mainSrc = join(sandbox, "packages", "with-interface", "src");

        // Rename both files together (proper rename, no orphan)
        await rename(
          join(mainSrc, "ModuleWithInterface.res"),
          join(mainSrc, "ModuleWithInterface2.res"),
        );
        await rename(
          join(mainSrc, "ModuleWithInterface.resi"),
          join(mainSrc, "ModuleWithInterface2.resi"),
        );

        // Wait for rebuild
        await watchHandle.waitForBuild();
      } finally {
        watchHandle.stop();
      }
    }));

  it("clean + build recovers when orphaned .resi is also renamed", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      const mainSrc = join(sandbox, "packages", "with-interface", "src");

      // Initial build
      await build(sandbox);

      // Rename only .res (create orphan)
      await rename(
        join(mainSrc, "ModuleWithInterface.res"),
        join(mainSrc, "ModuleWithInterface2.res"),
      );
      await clean(sandbox);
      await build(sandbox);

      // Now rename the .resi to match (fix the orphan)
      await rename(
        join(mainSrc, "ModuleWithInterface.resi"),
        join(mainSrc, "ModuleWithInterface2.resi"),
      );
      await clean(sandbox);
      await build(sandbox);
    }));

  it("clean + build handles deleting orphaned .resi", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      const mainSrc = join(sandbox, "packages", "with-interface", "src");

      // Initial build
      await build(sandbox);

      // Rename .res (orphan the .resi)
      await rename(
        join(mainSrc, "ModuleWithInterface.res"),
        join(mainSrc, "ModuleWithInterface2.res"),
      );
      await clean(sandbox);
      await build(sandbox);

      // Delete the orphaned .resi
      await rm(join(mainSrc, "ModuleWithInterface.resi"));
      await clean(sandbox);
      await build(sandbox);
    }));
});
