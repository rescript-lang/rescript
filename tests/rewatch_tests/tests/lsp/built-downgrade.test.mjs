import { existsSync } from "node:fs";
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
        let appDiag = diagnostics.find((d) => d.file === "src/App.res");
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
        appDiag = diagnostics.find((d) => d.file === "src/App.res");
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
});
