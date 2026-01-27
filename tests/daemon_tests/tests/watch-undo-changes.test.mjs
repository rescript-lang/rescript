import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests that undoing changes in watch mode leaves no stale artifacts.
// The daemon's incremental rebuild should correctly handle "undo" scenarios
// without accumulating stale state or leftover .mjs files.
//
// Source: EXPERIMENT_DAEMON.md "Future Test Scenarios" - "Undo Changes Leaves No Artifacts"

describe("watch-undo-changes", () => {
  it("undoing changes leaves output identical to initial state", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      const watchHandle = await watch(sandbox);

      const libraryResPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      const libraryMjsPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.mjs",
      );

      // Record initial state
      const originalSource = await readFile(libraryResPath, "utf8");
      const initialMjs = await readFile(libraryMjsPath, "utf8");

      // Append code to the file
      const modifiedSource = originalSource + '\nlet _ = Console.log("test")\n';
      await writeFile(libraryResPath, modifiedSource);

      // Wait for rebuild after modification
      await watchHandle.waitForBuild();

      // Verify the .mjs file changed
      const modifiedMjs = await readFile(libraryMjsPath, "utf8");
      expect(modifiedMjs).not.toBe(initialMjs);
      expect(modifiedMjs).toContain("test");

      // Undo the change - restore original source
      await writeFile(libraryResPath, originalSource);

      // Wait for rebuild after undo
      await watchHandle.waitForBuild();

      // Verify the .mjs file is identical to the initial state
      const finalMjs = await readFile(libraryMjsPath, "utf8");
      expect(finalMjs).toBe(initialMjs);

      watchHandle.stop();
    }));
});
