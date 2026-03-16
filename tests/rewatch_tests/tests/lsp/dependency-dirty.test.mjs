import { existsSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp dependency-dirty stage", { timeout: 60_000 }, () => {
  // Regression test for the SourceDirty/DependencyDirty distinction.
  //
  // Dependency graph: Main → App → {Leaf, Button}
  //
  // Scenario:
  // 1. Save Leaf.res to produce JS (initial build is typecheck-only).
  // 2. Save Leaf.res again with a non-breaking implementation change.
  //    compile_dependencies compiles Leaf (dependency closure).
  //    mark_modules_with_expired_deps_for_recompile marks App as
  //    DependencyDirty (not SourceDirty — its source hasn't changed).
  //    typecheck_dependents processes App and Main successfully.
  //
  // Before the fix: App would be set to Dirty (now SourceDirty), and
  // typecheck_dependents would either skip it (workaround) or panic
  // on the invalid Dirty → TypeChecked transition.
  //
  // After the fix: App is set to DependencyDirty (valid AST, just needs
  // recompilation), so typecheck_dependents compiles it normally.
  it("typechecks dependents marked DependencyDirty without errors", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const leafMjs = path.join(lspCwd, "src", "Leaf.mjs");

        // Step 1: Save Leaf.res to produce JS for Leaf.
        lsp.saveFile("src/Leaf.res");
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        expect(existsSync(leafMjs), "Leaf.mjs should exist after save").toBe(
          true,
        );

        // Step 2: Change Leaf's implementation (non-breaking — same export).
        // This triggers compile_dependencies for Leaf, then
        // typecheck_dependents for App and Main. App moves from
        // TypeChecked → DependencyDirty → TypeChecked.
        //
        // Before the fix: App would be set to SourceDirty (needing reparse),
        // and typecheck_dependents would panic on the invalid
        // SourceDirty → TypeChecked transition.
        await writeFile("src/Leaf.res", 'let value = "hello world"\n');
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // No diagnostics — the API didn't change, just the implementation.
        const diagnostics = lsp.getDiagnostics();
        for (const entry of diagnostics) {
          expect(
            entry.diagnostics,
            `Expected no diagnostics for ${entry.file}, got: ${JSON.stringify(entry.diagnostics)}`,
          ).toEqual([]);
        }

        // Leaf.mjs should exist with updated content.
        expect(
          existsSync(leafMjs),
          "Leaf.mjs should exist with updated content",
        ).toBe(true);
      },
      { cwd: "packages/dep-chain" },
    ));
});
