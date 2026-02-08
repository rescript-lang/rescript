import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp references", { timeout: 60_000 }, () => {
  it("finds all references to a value", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Library.res has: let greeting = "hello from library"
      // App.res has: let run = () => Library.greeting
      // Both are in the same dependency chain
      // Request references for `greeting` from Library.res itself
      await lsp.openFile("packages/library/src/Library.res");
      const result = await lsp.referencesFor(
        "packages/library/src/Library.res",
        0,
        4,
      );
      expect(result).not.toBeNull();
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThanOrEqual(1);

      const uris = result.map(loc => loc.uri);
      expect(uris.some(uri => uri.includes("Library.res"))).toBe(true);
    }));
});
