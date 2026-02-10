import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp definition", { timeout: 60_000 }, () => {
  it("jumps to definition of a module value", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Root.res contains: let main = App.run()
      lsp.openFile("src/Root.res");
      await lsp.waitForNotification("textDocument/publishDiagnostics");
      // Hover over `run` in `App.run()` at column 15
      const result = await lsp.definitionFor("src/Root.res", 0, 15);
      expect(result).not.toBeNull();
      expect(result.uri).toContain("App.res");
      expect(result.range.start.line).toBe(0);
    }));

  it("jumps to definition of a module", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Root.res contains: let main = App.run()
      lsp.openFile("src/Root.res");
      await lsp.waitForNotification("textDocument/publishDiagnostics");
      // Cursor on `App` in `App.run()` at column 11
      const result = await lsp.definitionFor("src/Root.res", 0, 11);
      expect(result).not.toBeNull();
      expect(result.uri).toContain("App.res");
    }));
});
