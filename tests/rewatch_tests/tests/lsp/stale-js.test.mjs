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
        expect(
          existsSync(buttonMjs),
          "Button.mjs should exist after fix",
        ).toBe(true);
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
});
