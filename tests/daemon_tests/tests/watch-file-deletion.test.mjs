import { join } from "node:path";
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests file deletion scenarios. The daemon holds the module graph in memory,
// so deleting a source file must correctly invalidate that module and report
// dependents as broken. Recovery after restoring the file should also work.
//
// Fixture dependency chain: Root -> App -> Library
//
// For CLI builds: the daemon reuses cached state between builds, so a clean
// is needed to force re-scanning sources after file deletion.
// For watch mode: the file watcher sends NotifyFileChange which triggers
// source re-scan automatically.

describe("watch-file-deletion", () => {
  it("clean + build fails when a depended-upon file is deleted", () =>
    runDaemonTest(async ({ sandbox, build, clean, deleteFile }) => {
      // Initial build should succeed
      await build(sandbox);

      // Delete Library.res — App depends on it
      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await deleteFile(libraryPath);

      // Clean to invalidate cached state, then rebuild
      await clean(sandbox);
      await build(sandbox);
    }));

  it("clean + build recovers after restoring a deleted file", () =>
    runDaemonTest(async ({ sandbox, build, clean, deleteFile, writeFile }) => {
      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );

      // Initial build
      await build(sandbox);

      // Delete, clean, rebuild (should fail)
      await deleteFile(libraryPath);
      await clean(sandbox);
      await build(sandbox);

      // Restore, clean, rebuild (should succeed)
      await writeFile(libraryPath, 'let greeting = "restored"\n');
      await clean(sandbox);
      await build(sandbox);
    }));

  it("deletion detected in watch mode triggers failed rebuild", () =>
    runDaemonTest(async ({ sandbox, watch, deleteFile }) => {
      const watcher = await watch(sandbox);

      // Delete Library.res while watch is running
      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await deleteFile(libraryPath);

      // Wait for the rebuild triggered by deletion
      await watcher.waitForBuild();
      watcher.stop();
    }));

  it("watch mode recovers after restoring a deleted file", () =>
    runDaemonTest(async ({ sandbox, watch, deleteFile, writeFile }) => {
      const watcher = await watch(sandbox);

      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );

      // Delete — should trigger failed rebuild
      await deleteFile(libraryPath);
      await watcher.waitForBuild();

      // Restore — should trigger successful rebuild
      await writeFile(libraryPath, 'let greeting = "restored"\n');
      await watcher.waitForBuild();

      watcher.stop();
    }));
});
