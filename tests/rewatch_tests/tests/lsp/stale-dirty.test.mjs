import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp stale dirty modules", { timeout: 60_000 }, () => {
  // Regression test for stale dirty dependents leaking into subsequent builds.
  //
  // Dependency graph: Main → App → Leaf, App → Button
  //
  // Scenario:
  // 1. Save Leaf.res with a broken API (rename `value` → `valueRenamed`).
  //    typecheck_dependents runs for [App, Main] and fails because
  //    App.res uses Leaf.value. App and Main are left at CompilationStage::Dirty.
  //
  // 2. Fix Leaf.res (restore `value`). compile_dependencies computes
  //    closure = [Leaf]. The promote loop should promote App (TypeChecked)
  //    to Built so it stays out of the compile universe.
  //
  //    BUG: Without the fix, App is still Dirty from step 1. It leaks
  //    into the compile universe and gets compiled even though it's not
  //    in the dependency closure.
  //
  //    FIX: The compile loop in compile() snapshots module stages before
  //    the loop and restores any modules still at Dirty that weren't
  //    dirty before the loop started.
  it("does not leak stale dirty dependents into subsequent builds", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Step 1: Break Leaf's API — App.res uses Leaf.value which no longer exists.
        await writeFile("src/Leaf.res", 'let valueRenamed = "hello"\n');
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // App.res should have diagnostics (unbound value Leaf.value)
        let diagnostics = lsp.getDiagnostics();
        const appDiag = diagnostics.find(d => d.file === "src/App.res");
        expect(appDiag, "Expected diagnostics for App.res").toBeDefined();
        expect(appDiag.diagnostics.length).toBeGreaterThan(0);

        // Step 2: Fix Leaf's API — restore the original export.
        await writeFile("src/Leaf.res", 'let value = "hello"\n');
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // All diagnostics should be cleared — no stale errors.
        diagnostics = lsp.getDiagnostics();
        for (const entry of diagnostics) {
          expect(
            entry.diagnostics,
            `Expected no diagnostics for ${entry.file}, got: ${JSON.stringify(entry.diagnostics)}`,
          ).toEqual([]);
        }
      },
      { cwd: "packages/dep-chain" },
    ));
});
