import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp completion", { timeout: 60_000 }, () => {
  it("returns completions for a module dot access", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Open the file, then type "App." to trigger module completion
      lsp.openFile("src/Root.res");
      lsp.editFile("src/Root.res", "let x = App.\n");

      // Wait for didChange typecheck to complete (produces .cmt)
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      const items = await lsp.completeFor("src/Root.res", 0, 12);
      const labels = items.map(i => i.label);
      // App module exports `run`
      expect(labels).toContain("run");
    }));

  it("returns completions for stdlib modules like Console", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Open the file, then type "Console." to trigger completion on a stdlib module
      lsp.openFile("src/Root.res");
      lsp.editFile("src/Root.res", "Console.\n");

      // Wait for didChange typecheck to complete
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      const items = await lsp.completeFor("src/Root.res", 0, 8);
      const labels = items.map(i => i.label);
      expect(labels.length).toBeGreaterThan(1);
      expect(labels).toContain("log");
    }));

  it("resolves documentation for a file module completion", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Open App.res and type "Lib" to get Library as a FileModule completion
      lsp.openFile("packages/app/src/App.res");
      lsp.editFile("packages/app/src/App.res", "Lib\n");

      // Wait for didChange typecheck
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      const items = await lsp.completeFor("packages/app/src/App.res", 0, 3);
      // Find the Library completion item — it should have data with modulePath
      const libraryItem = items.find(
        i => i.label === "Library" && i.data?.modulePath,
      );
      expect(libraryItem).toBeDefined();
      expect(libraryItem.data.modulePath).toBe("Library");

      // Resolve it — should populate documentation
      const resolved = await lsp.resolveCompletion(libraryItem);
      expect(resolved.documentation).toBe("A library for greetings and users.");
    }));
});
