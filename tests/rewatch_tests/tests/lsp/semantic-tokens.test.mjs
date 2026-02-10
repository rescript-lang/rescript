import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp semantic tokens", { timeout: 60_000 }, () => {
  it("returns semantic tokens for a file", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Library.res contains:
      //   let greeting = "hello from library"
      //   type user = {name: string}
      //   let admin: user = {name: "admin"}
      //   let greet = (name: string) => "hello " ++ name
      lsp.openFile("packages/library/src/Library.res");
      await lsp.waitForNotification("textDocument/publishDiagnostics");
      const result = await lsp.semanticTokensFor(
        "packages/library/src/Library.res",
      );
      expect(result).not.toBeNull();
      expect(result.data).toBeDefined();
      expect(Array.isArray(result.data)).toBe(true);
      expect(result.data.length).toBeGreaterThan(0);
    }));
});
