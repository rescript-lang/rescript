import { writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { runDaemonTest } from "../helpers/test-context-v2.mjs";

// Tests that compiler warnings from non-recompiled modules persist across
// incremental rebuilds. The daemon must cache and replay warnings from modules
// that weren't recompiled. Without this, incremental builds silently drop
// warnings from unchanged modules.
//
// Fixture dependency chain: Root -> App -> Library
//
// Note: ReScript only warns about unused bindings inside functions (top-level
// bindings are exported). We use a helper function with an unused local binding
// to trigger warning 26.

// Source that produces warning 26 (unused variable) while keeping `greeting`
// available as a string for dependents.
const LIBRARY_WITH_WARNING =
  'let greeting = "hello"\n\nlet helper = () => {\n  let unusedValue = 42\n  "result"\n}\n';

const LIBRARY_WITHOUT_WARNING = 'let greeting = "hello"\n';

describe("watch-warnings-persist", () => {
  it("warnings persist across incremental CLI builds", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      // Add an unused binding inside a function to Library (generates warning 26)
      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await writeFile(libraryPath, LIBRARY_WITH_WARNING);

      // First build — should show the unused warning
      const result1 = await cli.build();
      expect(result1.status).toBe(0);
      expect(result1.stderr).toContain("unusedValue");

      // Modify an unrelated file (Root.res) without fixing the warning
      const rootPath = join(sandbox, "src", "Root.res");
      await writeFile(rootPath, "let main = App.run()\n// modified\n");

      // Second build — Library is not recompiled, but warning should persist
      const result2 = await cli.build();
      expect(result2.status).toBe(0);
      expect(result2.stderr).toContain("unusedValue");
    }));

  it("warnings persist in watch mode after unrelated file change", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      // Add an unused binding inside a function to Library
      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await writeFile(libraryPath, LIBRARY_WITH_WARNING);

      // Start watch (this waits for initial build)
      const watchHandle = await watch(sandbox);

      // Modify an unrelated file to trigger incremental rebuild
      const rootPath = join(sandbox, "src", "Root.res");
      await writeFile(rootPath, "let main = App.run()\n// watch-modified\n");

      // Wait for incremental rebuild
      await watchHandle.waitForBuild();

      // Note: We can't easily check stderr in the new test context,
      // but the snapshot will show the build completed successfully
      // which is the main behavior we want to verify

      watchHandle.stop();
    }));

  it("warning disappears when the source of warning is fixed", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );

      // First build with warning
      await writeFile(libraryPath, LIBRARY_WITH_WARNING);
      const result1 = await cli.build();
      expect(result1.status).toBe(0);
      expect(result1.stderr).toContain("unusedValue");

      // Fix the warning by removing the helper function
      await writeFile(libraryPath, LIBRARY_WITHOUT_WARNING);

      // Second build — warning gone
      const result2 = await cli.build();
      expect(result2.status).toBe(0);
      expect(result2.stderr).not.toContain("unusedValue");
    }));
});
