import { existsSync, readFileSync, writeFileSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp compile mode preservation", { timeout: 60_000 }, () => {
  // Regression test: CompileError(FullCompile) must not be downgraded
  // to CompileError(TypecheckOnly) by an intervening typecheck pass.
  //
  // Dependency graph: Main → App → {Leaf, Button}
  //
  // Scenario:
  // 1. Save App.res with a type error (call Button.label as function).
  //    App fails compilation → CompileError { compile_mode: FullCompile }.
  //    The user wants JS for App once the error is resolved.
  //
  // 2. Save Leaf.res with a harmless change. typecheck_dependents
  //    retypechecks App (still broken) → CompileError again.
  //    BUG: without the fix, this overwrites compile_mode to TypecheckOnly.
  //
  // 3. Save Button.res to fix the API (make label a function).
  //    typecheck_dependents typechecks App → succeeds → TypeChecked.
  //    compile_resolved_errors should pick up App (was FullCompile error)
  //    and emit JS.
  //
  // Without the fix, step 2 downgrades the compile_mode and step 3
  // skips JS emission — App.mjs is never produced.
  it("preserves FullCompile intent across intervening typecheck passes", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const appMjs = path.join(lspCwd, "src", "App.mjs");

        // Step 1: Save App.res with a type error — Button.label is a
        // string, not a function. App fails with CompileError(FullCompile).
        await writeFile(
          "src/App.res",
          'let run = () => (Leaf.value, Button.label("Hello"))\n',
        );
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // App.res should have diagnostics
        let diagnostics = lsp.getDiagnostics();
        let appDiag = diagnostics.find(d => d.file === "src/App.res");
        expect(appDiag, "Expected diagnostics for App.res").toBeDefined();
        expect(appDiag.diagnostics.length).toBeGreaterThan(0);

        // App.mjs should not exist — compilation failed
        expect(
          existsSync(appMjs),
          "App.mjs should not exist after failed save",
        ).toBe(false);

        // Step 2: Save Leaf.res with a harmless change (unrelated to the error).
        // This triggers typecheck_dependents which retypechecks App — it still
        // fails, but the CompileError must preserve compile_mode: FullCompile.
        await writeFile("src/Leaf.res", 'let value = "hello world"\n');
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // App.res should still have diagnostics
        diagnostics = lsp.getDiagnostics();
        appDiag = diagnostics.find(d => d.file === "src/App.res");
        expect(
          appDiag,
          "Expected App.res to still have diagnostics after unrelated save",
        ).toBeDefined();
        expect(appDiag.diagnostics.length).toBeGreaterThan(0);

        // Step 3: Fix the API — make Button.label a function so App.res compiles.
        await writeFile(
          "src/Button.res",
          'let label = arg => "click me" ++ arg\n',
        );
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // All diagnostics should be cleared
        diagnostics = lsp.getDiagnostics();
        for (const entry of diagnostics) {
          expect(
            entry.diagnostics,
            `Expected no diagnostics for ${entry.file}, got: ${JSON.stringify(entry.diagnostics)}`,
          ).toEqual([]);
        }

        // App.mjs MUST exist — the FullCompile intent from step 1 should
        // have been preserved through step 2's typecheck pass, and step 3's
        // compile_resolved_errors should have emitted JS.
        expect(
          existsSync(appMjs),
          "App.mjs should exist — FullCompile intent must survive intervening typecheck",
        ).toBe(true);
      },
      { cwd: "packages/dep-chain" },
    ));

  // Regression test: cross-flush delete+create (LLM file overwrite) must
  // produce JS output, not trigger two TypecheckOnly project rebuilds.
  //
  // Scenario:
  // 1. Save Leaf.res to produce Leaf.mjs (initial build is TypecheckOnly).
  // 2. Delete Leaf.res and notify as Deleted — triggers project_build flush.
  // 3. Wait for the delete to land in a separate flush (200ms > debounce).
  // 4. Write new content and notify as Created — should be treated as save.
  // 5. Verify Leaf.mjs exists with the updated content.
  it("external file overwrite via delete+create still produces JS", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile, deleteFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Save Leaf.res to produce Leaf.mjs (initial build is TypecheckOnly, no JS yet)
        await writeFile("src/Leaf.res", 'let value = "hello"\n');
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const leafMjs = path.join(lspCwd, "src", "Leaf.mjs");
        expect(existsSync(leafMjs), "Leaf.mjs should exist after save").toBe(
          true,
        );

        // Simulate LLM-style file overwrite: delete then create in separate flushes.
        await deleteFile("src/Leaf.res");
        lsp.notifyWatchedFilesChanged([
          { relativePath: "src/Leaf.res", type: 3 }, // Deleted
        ]);

        // Wait for the delete flush to complete
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Write new content and notify as created
        const fullPath = path.join(lspCwd, "src", "Leaf.res");
        writeFileSync(fullPath, 'let value = "updated"\n');
        lsp.notifyWatchedFilesChanged([
          { relativePath: "src/Leaf.res", type: 1 }, // Created
        ]);

        // Wait for the create flush to complete
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Leaf.mjs should exist with updated content.
        // Without the fix, the delete triggers project_build (TypecheckOnly),
        // and the create triggers another project_build — neither emits JS.
        // With the fix, the cross-flush delete+create is detected and treated
        // as a save, which goes through file_build (FullCompile) → JS emitted.
        expect(
          existsSync(leafMjs),
          "Leaf.mjs must exist after delete+create overwrite",
        ).toBe(true);
        const content = readFileSync(leafMjs, "utf8");
        expect(content).toContain("updated");
      },
      { cwd: "packages/dep-chain" },
    ));
});
