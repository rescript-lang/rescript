import { existsSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp stale JS output", { timeout: 60_000 }, () => {
  // Regression test for stale JS when a dependency fix should regenerate
  // JS for both the dependency and its dependent.
  //
  // Dependency graph: Main → App → {Leaf, Button}
  //
  // Scenario:
  // 1. Save App.res with `Button.label("Hello")` — this breaks because
  //    Button.label is a string, not a function. App gets a compile error.
  //
  // 2. Save Button.res with `let label = arg => "click me" ++ arg` —
  //    this fixes the API so App.res is now valid.
  //
  // Expected: Both App.res and Button.res produce fresh JS.
  // Actual (bug): App.res JS is not regenerated — it only gets typechecked.
  it("regenerates JS for dependents when a dependency fix resolves their errors", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const appMjs = path.join(lspCwd, "src", "App.mjs");
        const buttonMjs = path.join(lspCwd, "src", "Button.mjs");

        // Step 1: Break App.res — call Button.label as a function.
        // Button.label is a string, so this is a type error.
        await writeFile(
          "src/App.res",
          'let run = () => (Leaf.value, Button.label("Hello"))\n',
        );
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // App.res should have diagnostics (type error: string applied to argument)
        let diagnostics = lsp.getDiagnostics();
        const appDiag = diagnostics.find(d => d.file === "src/App.res");
        expect(appDiag, "Expected diagnostics for App.res").toBeDefined();
        expect(appDiag.diagnostics.length).toBeGreaterThan(0);

        // Step 2: Fix Button.res — change label from string to function.
        // This makes App.res valid again.
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

        // Both files should have JS output
        expect(existsSync(buttonMjs), "Button.mjs should exist after fix").toBe(
          true,
        );
        expect(
          existsSync(appMjs),
          "App.mjs should exist after fix (dependent JS should be regenerated)",
        ).toBe(true);
      },
      { cwd: "packages/dep-chain" },
    ));

  // Verify that saving an unrelated file does NOT trigger compile_resolved
  // for an errored module. Only saving the dependency that caused the error
  // should resolve it.
  //
  // Scenario:
  // 1. Save App.res with `Button.label("Hello")` — type error (same as above).
  // 2. Save Leaf.res with a harmless change — unrelated to App's error.
  //
  // Expected: App.res keeps its diagnostics and no compile_resolved fires.
  it("does not recompile errored dependents when saving an unrelated file", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const appMjs = path.join(lspCwd, "src", "App.mjs");

        // Step 1: Break App.res — call Button.label as a function.
        await writeFile(
          "src/App.res",
          'let run = () => (Leaf.value, Button.label("Hello"))\n',
        );
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // App.res should have diagnostics
        let diagnostics = lsp.getDiagnostics();
        const appDiag = diagnostics.find(d => d.file === "src/App.res");
        expect(appDiag, "Expected diagnostics for App.res").toBeDefined();
        expect(appDiag.diagnostics.length).toBeGreaterThan(0);

        // Step 2: Save Leaf.res with a harmless change — unrelated to the error.
        await writeFile("src/Leaf.res", 'let value = "hello world"\n');
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // App.res should still have diagnostics
        diagnostics = lsp.getDiagnostics();
        const appDiagAfter = diagnostics.find(d => d.file === "src/App.res");
        expect(
          appDiagAfter,
          "Expected App.res to still have diagnostics after saving unrelated file",
        ).toBeDefined();
        expect(appDiagAfter.diagnostics.length).toBeGreaterThan(0);

        // App.mjs should NOT exist — the error is unresolved
        expect(
          existsSync(appMjs),
          "App.mjs should not exist — error is unresolved",
        ).toBe(false);
      },
      { cwd: "packages/dep-chain" },
    ));

  // Regression test for skipped modules in the dependency closure.
  //
  // Dependency graph: Main → App → {Leaf, Button}
  //
  // Scenario:
  // 1. Break Leaf.res with a type error (introduce an unknown module ref).
  //    Save Leaf.res so the error is in the build state.
  //
  // 2. Save App.res (unchanged content) — App depends on Leaf.
  //    compile_dependencies builds the closure {Leaf, Button, App}.
  //    Leaf has a compile error → the compile loop breaks early.
  //    App is never attempted by bsc — it's "skipped".
  //
  // 3. Fix Leaf.res and save it.
  //
  // Expected: App.mjs is produced — the skipped module was registered as
  // FullCompile intent and gets compiled when the dependency is fixed.
  // Before the fix, App was stuck at TypeChecked with no JS output.
  it("produces JS for skipped modules when a dependency error is resolved", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const appMjs = path.join(lspCwd, "src", "App.mjs");
        const leafMjs = path.join(lspCwd, "src", "Leaf.mjs");

        // Step 1: Break Leaf.res — reference an unknown module.
        await writeFile("src/Leaf.res", "let value = Unknown.stuff\n");
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Leaf.res should have diagnostics
        let diagnostics = lsp.getDiagnostics();
        const leafDiag = diagnostics.find(d => d.file === "src/Leaf.res");
        expect(leafDiag, "Expected diagnostics for Leaf.res").toBeDefined();
        expect(leafDiag.diagnostics.length).toBeGreaterThan(0);

        // Step 2: Save App.res (valid content, but depends on broken Leaf).
        // App's dependency closure includes Leaf, so compile_dependencies
        // will fail and App will be skipped — never attempted by bsc.
        await writeFile(
          "src/App.res",
          "let run = () => (Leaf.value, Button.label)\n",
        );
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // App.mjs should NOT exist — Leaf's error prevented compilation.
        expect(
          existsSync(appMjs),
          "App.mjs should not exist while dependency has errors",
        ).toBe(false);

        // Step 3: Fix Leaf.res.
        await writeFile("src/Leaf.res", 'let value = "fixed"\n');
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // All diagnostics should be cleared
        diagnostics = lsp.getDiagnostics();
        for (const entry of diagnostics) {
          expect(
            entry.diagnostics,
            `Expected no diagnostics for ${entry.file}, got: ${JSON.stringify(entry.diagnostics)}`,
          ).toEqual([]);
        }

        // Both Leaf.mjs and App.mjs should exist — the skipped module
        // was registered as intent and compiled when Leaf was fixed.
        expect(existsSync(leafMjs), "Leaf.mjs should exist after fix").toBe(
          true,
        );
        expect(
          existsSync(appMjs),
          "App.mjs should exist after dependency fix (skipped module intent)",
        ).toBe(true);
      },
      { cwd: "packages/dep-chain" },
    ));
});
