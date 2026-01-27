import { rename } from "node:fs/promises";
import { join } from "node:path";
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests file rename scenarios in namespaced packages. Namespaces add complexity
// because module names are scoped within the namespace, affecting how dependent
// modules resolve references.
//
// Fixture: packages/namespaced has namespace "DaemonTestNS" with:
//   - Helper.res: exports greet()
//   - Consumer.res: calls Helper.greet()

describe("watch-file-rename-namespace", () => {
  it("clean + build fails when a depended-upon file is renamed in namespaced package", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      // Initial build should succeed
      await build(sandbox);

      // Rename Helper.res → Helper2.res (module name changes, breaking Consumer)
      const nsDir = join(sandbox, "packages", "namespaced", "src");
      await rename(join(nsDir, "Helper.res"), join(nsDir, "Helper2.res"));

      // Clean to invalidate cached state, then rebuild
      await clean(sandbox);
      await build(sandbox);
    }));

  it("clean + build recovers when renamed file is renamed back in namespaced package", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      const nsDir = join(sandbox, "packages", "namespaced", "src");

      // Initial build
      await build(sandbox);

      // Rename (break it), clean, rebuild
      await rename(join(nsDir, "Helper.res"), join(nsDir, "Helper2.res"));
      await clean(sandbox);
      await build(sandbox);

      // Rename back (fix it), clean, rebuild
      await rename(join(nsDir, "Helper2.res"), join(nsDir, "Helper.res"));
      await clean(sandbox);
      await build(sandbox);
    }));

  it("rename detected in watch mode triggers failed rebuild in namespaced package", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      // Start watch (waits for initial build)
      const watchHandle = await watch(sandbox);

      try {
        // Rename Helper.res while watch is running
        const nsDir = join(sandbox, "packages", "namespaced", "src");
        await rename(join(nsDir, "Helper.res"), join(nsDir, "Helper2.res"));

        // Wait for the rebuild triggered by the rename
        await watchHandle.waitForBuild();
      } finally {
        watchHandle.stop();
      }
    }));

  it("watch mode recovers after renaming file back in namespaced package", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      // Start watch (waits for initial build)
      const watchHandle = await watch(sandbox);

      try {
        const nsDir = join(sandbox, "packages", "namespaced", "src");

        // Rename — should trigger failed rebuild
        await rename(join(nsDir, "Helper.res"), join(nsDir, "Helper2.res"));
        await watchHandle.waitForBuild();

        // Rename back — should trigger successful rebuild
        await rename(join(nsDir, "Helper2.res"), join(nsDir, "Helper.res"));
        await watchHandle.waitForBuild();
      } finally {
        watchHandle.stop();
      }
    }));
});
