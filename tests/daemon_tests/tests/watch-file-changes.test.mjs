import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests that watch mode responds to file changes. The watch client uses
// notify-rs to detect file system changes and sends NotifyFileChange RPCs to
// the daemon, which triggers incremental rebuilds.
//
// The initial build writes in-source .mjs files which the file watcher picks
// up, causing a cascade of rebuilds. Tests wait for the cascade to settle
// before making modifications.

describe("watch-file-changes", () => {
  it("rebuilds when a source file is modified", () =>
    runDaemonTest(async ({ sandbox, watch, writeFile }) => {
      const watcher = await watch(sandbox);

      // Modify a source file
      const filePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await writeFile(filePath, 'let greeting = "modified"\n');

      // Wait for rebuild triggered by file change
      await watcher.waitForBuild();
      watcher.stop();
    }));

  it("reports errors on file change that introduces a syntax error", () =>
    runDaemonTest(async ({ sandbox, watch, writeFile }) => {
      const watcher = await watch(sandbox);

      // Introduce a syntax error
      const filePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await writeFile(filePath, "let greeting = {\n");

      // Wait for rebuild (which should fail)
      await watcher.waitForBuild();
      watcher.stop();
    }));

  it("recovers after fixing an error in watch mode", () =>
    runDaemonTest(async ({ sandbox, watch, writeFile }) => {
      const watcher = await watch(sandbox);

      const filePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );

      // Break it
      await writeFile(filePath, "let greeting = {\n");
      await watcher.waitForBuild();

      // Fix it
      await writeFile(filePath, 'let greeting = "fixed"\n');
      await watcher.waitForBuild();

      watcher.stop();
    }));

  it("build command works while watch is running", () =>
    runDaemonTest(async ({ sandbox, build, watch }) => {
      const watcher = await watch(sandbox);

      // Run a build command concurrently — this is the key daemon use case
      await build(sandbox);

      watcher.stop();
    }));

  it("rebuilds when a new source file is created", () =>
    runDaemonTest(async ({ sandbox, watch, writeFile, fileExists }) => {
      const watcher = await watch(sandbox);

      const newFilePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "NewModule.res",
      );

      // Create a new source file
      await writeFile(newFilePath, 'let value = "new module"\n');

      // Wait for rebuild
      await watcher.waitForBuild();

      // Verify the new module's artifact was created
      const newModuleArtifact = join(
        sandbox,
        "packages",
        "library",
        "src",
        "NewModule.mjs",
      );
      expect(fileExists(newModuleArtifact)).toBe(true);

      watcher.stop();
    }));

  it("handles rapid file changes by debouncing", () =>
    runDaemonTest(
      async ({ sandbox, watch, writeFile }) => {
        const watcher = await watch(sandbox);

        const filePath = join(
          sandbox,
          "packages",
          "library",
          "src",
          "Library.res",
        );

        // Make rapid successive changes (simulates editor auto-save, formatter, etc.)
        await writeFile(filePath, 'let greeting = "change1"\n');
        await writeFile(filePath, 'let greeting = "change2"\n');
        await writeFile(filePath, 'let greeting = "change3"\n');

        // Wait for at least one build
        await watcher.waitForBuild();
        // Let it settle
        await watcher.waitForSettle();

        watcher.stop();
      },
      {
        // Debouncing may coalesce changes, so just verify at least 1 file change build
        processSpans: summary => {
          const fileChangeBuildCount = summary.filter(s =>
            s.includes("work_queue.handle_file_change_build"),
          ).length;
          // Replace variable number of builds with a range assertion in the summary
          return summary.map(line =>
            line.includes("work_queue.handle_file_change_build")
              ? line.replace(
                  /work_queue\.handle_file_change_build/,
                  `work_queue.handle_file_change_build (${fileChangeBuildCount >= 1 ? "1+" : "0"} builds)`,
                )
              : line,
          );
        },
      },
    ));
});
