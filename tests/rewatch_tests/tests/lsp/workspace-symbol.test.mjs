import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp workspace symbol", { timeout: 60_000 }, () => {
  it("finds symbols matching a query", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      const result = await lsp.workspaceSymbolsFor("greeting");
      expect(result).not.toBeNull();
      expect(Array.isArray(result)).toBe(true);

      const names = result.map(s => s.name);
      expect(names).toContain("greeting");
    }));

  it("returns null for a query with no matches", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      const result = await lsp.workspaceSymbolsFor("zzz_nonexistent_zzz");
      expect(result).toBeNull();
    }));

  it("returns symbols across multiple files", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // "run" is in App.res, "greeting" is in Library.res
      const result = await lsp.workspaceSymbolsFor("r");
      expect(result).not.toBeNull();
      expect(Array.isArray(result)).toBe(true);

      const names = result.map(s => s.name);
      // Should find symbols from multiple files
      expect(names).toContain("run");
      expect(names).toContain("greet");
    }));

  it("finds type symbols and record fields", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      const result = await lsp.workspaceSymbolsFor("user");
      expect(result).not.toBeNull();

      const userSymbol = result.find(
        s => s.name === "user" && s.containerName === "Library",
      );
      expect(userSymbol).toBeDefined();
      // TypeParameter kind = 26
      expect(userSymbol.kind).toBe(26);
    }));
});
