import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests that the daemon does not enter an infinite loop when building after
// a file modification is reverted. This corresponds to bash test
// compile/13-no-infinite-loop-with-cycle.sh in rewatch/tests.
//
// The scenario:
// 1. Clean the project
// 2. Modify a file to add a dependency
// 3. Revert the modification
// 4. Build the project
// The build should complete without hanging in an infinite loop.

describe("build-no-infinite-loop", () => {
  it("completes build after file modification is reverted", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      // Clean the project first
      await clean(sandbox);

      // Read the original content of App.res
      const appPath = join(sandbox, "packages", "app", "src", "App.res");
      const originalContent = await readFile(appPath, "utf-8");

      // Modify the file to add a dependency that could create complexity
      // (similar to adding Dep01.log() in the original bash test)
      await writeFile(
        appPath,
        originalContent + "\nlet extra = Library.greeting\n",
      );

      // Revert the change
      await writeFile(appPath, originalContent);

      // Build should complete without hanging
      // The test will timeout if there's an infinite loop
      await build(sandbox);
    }));

  it("completes build after introducing and reverting a cycle-like change", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      // Clean the project first
      await clean(sandbox);

      // Read the original content of Library.res
      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      const originalContent = await readFile(libraryPath, "utf-8");

      // Modify Library to reference App (which would create a cycle since App depends on Library)
      await writeFile(
        libraryPath,
        originalContent + "\nlet cyclic = App.run()\n",
      );

      // Revert the change before building
      await writeFile(libraryPath, originalContent);

      // Build should complete without hanging
      await build(sandbox);
    }));
});
