import { existsSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp didSave", { timeout: 60_000 }, () => {
  it("produces JS output when a file is saved", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);

      // Wait for initial type-check-only build
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Verify no JS files exist after initial build (TypecheckOnly profile)
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "No .mjs should exist after initial build",
      ).toBe(false);

      // Save the file without changes — triggers TypecheckAndEmit build
      lsp.saveFile("src/Root.res");

      // Wait for the incremental build to finish
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Now JS files should exist for the saved file and its dependencies
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should exist after save",
      ).toBe(true);

      // Diagnostics should be clean
      const diagnostics = lsp.getDiagnostics();
      expect(diagnostics).toEqual([]);
    }));

  it("produces JS output for dependent files when a file is saved", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Save Root.res — triggers TypecheckAndEmit build
      lsp.saveFile("src/Root.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Dependencies should also have JS output
      expect(
        existsSync(path.join(sandbox, "packages", "app", "src", "App.mjs")),
        "App.mjs should exist after saving Root.res",
      ).toBe(true);
      expect(
        existsSync(
          path.join(sandbox, "packages", "library", "src", "Library.mjs"),
        ),
        "Library.mjs should exist after saving Root.res",
      ).toBe(true);
    }));

  it("does not compile unrelated files when a file is saved", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Save Unrelated.res — it has no dependencies, so only itself
      // should be compiled. Root, App, Library should be excluded.
      lsp.saveFile("packages/library/src/Unrelated.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Unrelated.mjs should exist (it was saved)
      expect(
        existsSync(
          path.join(sandbox, "packages", "library", "src", "Unrelated.mjs"),
        ),
        "Unrelated.mjs should exist after save",
      ).toBe(true);

      // Root.res is not in Unrelated's dependency closure —
      // it should NOT be compiled to JS
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should not exist (not in dependency closure)",
      ).toBe(false);
    }));

  it("preserves JS output from previous saves when saving an unrelated file", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // First save Root.res — compiles Root + its dependency closure
      lsp.saveFile("src/Root.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should exist after first save",
      ).toBe(true);

      // Now save Unrelated.res — Root is outside its closure but was
      // already Built. Its JS output and compilation stage must survive.
      lsp.saveFile("packages/library/src/Unrelated.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Root.mjs should still exist from the previous save
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should still exist after saving an unrelated file",
      ).toBe(true);
      expect(
        existsSync(
          path.join(sandbox, "packages", "library", "src", "Unrelated.mjs"),
        ),
        "Unrelated.mjs should exist after save",
      ).toBe(true);
    }));

  it("publishes diagnostics when saving a file with an error", () =>
    runLspTest(async ({ lsp, sandbox, writeFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Introduce a type error
      await writeFile("src/Root.res", "let main: int = App.run()\n");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Should have diagnostics for the error
      const diagnostics = lsp.getDiagnostics();
      const rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      expect(rootDiag, "Expected diagnostics for Root.res").toBeDefined();
      expect(rootDiag.diagnostics.length).toBeGreaterThan(0);
      expect(rootDiag.diagnostics[0].severity).toBe(1); // Error
    }));

  it("clears diagnostics when error is fixed and file is saved", () =>
    runLspTest(async ({ lsp, sandbox, writeFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Introduce a parse error
      await writeFile("src/Root.res", "let main = \n");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Should have diagnostics
      let diagnostics = lsp.getDiagnostics();
      let rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      expect(rootDiag, "Expected diagnostics for parse error").toBeDefined();
      expect(rootDiag.diagnostics.length).toBeGreaterThan(0);

      // Fix the error
      await writeFile("src/Root.res", "let main = App.run()\n");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Diagnostics should be cleared
      diagnostics = lsp.getDiagnostics();
      rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      if (rootDiag) {
        expect(rootDiag.diagnostics).toEqual([]);
      }

      // JS should exist after fixing
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should exist after fix",
      ).toBe(true);
    }));
});
