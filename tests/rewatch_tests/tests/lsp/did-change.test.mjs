import { existsSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp didChange", { timeout: 60_000 }, () => {
  it("publishes type diagnostics for unsaved changes", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Send unsaved change with a type error: App.run() returns string, not int
      lsp.editFile("src/Root.res", "let main: int = App.run()\n");

      // Wait for diagnostics to be published
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      const diagnostics = lsp.getDiagnostics();
      const rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      expect(rootDiag, "Expected diagnostics for Root.res").toBeDefined();
      expect(rootDiag.diagnostics.length).toBeGreaterThan(0);
      expect(rootDiag.diagnostics[0].severity).toBe(1); // Error
    }));

  it("clears diagnostics when unsaved change is fixed", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Introduce a type error
      lsp.editFile("src/Root.res", "let main: int = App.run()\n");
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      let diagnostics = lsp.getDiagnostics();
      let rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      expect(rootDiag, "Expected diagnostics for type error").toBeDefined();
      expect(rootDiag.diagnostics.length).toBeGreaterThan(0);

      // Fix the error
      lsp.editFile("src/Root.res", "let main = App.run()\n");
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      diagnostics = lsp.getDiagnostics();
      rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      if (rootDiag) {
        expect(rootDiag.diagnostics).toEqual([]);
      }
    }));

  it("does not produce JS output on didChange", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Send an unsaved change with a type error so we get diagnostics back
      // (confirms the typecheck ran) — then verify no JS was produced.
      lsp.editFile("src/Root.res", "let main: int = App.run()\n");
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "No .mjs should exist after didChange (TypecheckOnly)",
      ).toBe(false);
    }));

  it("publishes syntax error diagnostics for unsaved changes", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Incomplete expression — syntax error
      lsp.editFile("src/Root.res", "let main = \n");
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      const diagnostics = lsp.getDiagnostics();
      const rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      expect(rootDiag, "Expected diagnostics for syntax error").toBeDefined();
      expect(rootDiag.diagnostics.length).toBeGreaterThan(0);
      expect(rootDiag.diagnostics[0].severity).toBe(1); // Error
    }));
});
