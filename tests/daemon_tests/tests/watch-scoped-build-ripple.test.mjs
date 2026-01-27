import { join } from "node:path";
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests that a scoped build from a child package while watch mode is running
// does not cause unnecessary recompilation of unrelated packages.
//
// Scenario: Watch mode is running from root (all packages loaded and compiled).
// A scoped build clears all local package sources in the shared build state.
// When a file change then arrives via the watch client, the FileChangeBuild
// should only recompile the changed module — not rebuild everything from scratch.

describe("watch-scoped-build-ripple", () => {
  it("file change after scoped build does not cause full rebuild", () =>
    runDaemonTest(async ({ sandbox, build, watch, writeFile }) => {
      // 1. Start watch from root — loads all packages, compiles everything
      const watcher = await watch(sandbox);

      // 2. Run scoped build from library directory (library has no deps)
      //    This clears all local package sources in the shared build state.
      const libDir = join(sandbox, "packages", "library");
      await build(libDir);

      // 3. Modify a file in library — triggers FileChangeBuild via watch.
      //    Only Library should be recompiled, not all packages.
      const filePath = join(libDir, "src", "Library.res");
      await writeFile(filePath, 'let greeting = "modified"\n');
      await watcher.waitForBuild();

      watcher.stop();
    }));
});
