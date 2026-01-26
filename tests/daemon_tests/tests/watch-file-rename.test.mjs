import { rename, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context-v2.mjs";

// Tests file rename scenarios. A rename is effectively a delete + create at
// the filesystem level. The daemon must handle this correctly: the old module
// disappears and dependents break, while a new (unrelated) module appears.
//
// Fixture dependency chain: Root -> App -> Library
// App.res contains: let run = () => Library.greeting
//
// For CLI builds: the daemon reuses cached state between builds, so a clean
// is needed to force re-scanning sources after file rename.
// For watch mode: the file watcher detects rename as delete + create events.

describe("watch-file-rename", () => {
  it("clean + build fails when a depended-upon file is renamed", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      // Initial build should succeed
      await build(sandbox);

      // Rename Library.res → Library2.res (module name changes, breaking App)
      const libDir = join(sandbox, "packages", "library", "src");
      await rename(join(libDir, "Library.res"), join(libDir, "Library2.res"));

      // Clean to invalidate cached state, then rebuild
      await clean(sandbox);
      await build(sandbox);
    }));

  it("clean + build recovers when renamed file is renamed back", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      const libDir = join(sandbox, "packages", "library", "src");

      // Initial build
      await build(sandbox);

      // Rename (break it), clean, rebuild
      await rename(join(libDir, "Library.res"), join(libDir, "Library2.res"));
      await clean(sandbox);
      await build(sandbox);

      // Rename back (fix it), clean, rebuild
      await rename(join(libDir, "Library2.res"), join(libDir, "Library.res"));
      await clean(sandbox);
      await build(sandbox);
    }));

  it("rename detected in watch mode triggers failed rebuild", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      // Start watch (waits for initial build)
      const watchHandle = await watch(sandbox);

      try {
        // Rename Library.res while watch is running
        const libDir = join(sandbox, "packages", "library", "src");
        await rename(join(libDir, "Library.res"), join(libDir, "Library2.res"));

        // Wait for the rebuild triggered by the rename
        await watchHandle.waitForBuild();
      } finally {
        watchHandle.stop();
      }
    }));

  it("watch mode recovers after renaming file back", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      // Start watch (waits for initial build)
      const watchHandle = await watch(sandbox);

      try {
        const libDir = join(sandbox, "packages", "library", "src");

        // Rename — should trigger failed rebuild
        await rename(join(libDir, "Library.res"), join(libDir, "Library2.res"));
        await watchHandle.waitForBuild();

        // Rename back — should trigger successful rebuild
        await rename(join(libDir, "Library2.res"), join(libDir, "Library.res"));
        await watchHandle.waitForBuild();
      } finally {
        watchHandle.stop();
      }
    }));

  it("clean + build handles interface file rename without crash", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      const libDir = join(sandbox, "packages", "library", "src");

      // Add an interface file for Library
      await writeFile(join(libDir, "Library.resi"), "let greeting: string\n");

      // Build with interface — should succeed
      await build(sandbox);

      // Rename the interface file — orphans it from the implementation
      await rename(join(libDir, "Library.resi"), join(libDir, "Library2.resi"));

      // Clean and rebuild — should not crash (may succeed or fail depending on
      // how daemon handles orphaned .resi, but must not panic)
      await clean(sandbox);
      await build(sandbox);
    }));
});
