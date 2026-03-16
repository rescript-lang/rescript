import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp code lens", { timeout: 60_000 }, () => {
  it("returns code lenses for functions in a file", () =>
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
      const result = await lsp.codeLensFor("packages/library/src/Library.res");
      expect(result).not.toBeNull();
      expect(Array.isArray(result)).toBe(true);

      // Should have a code lens for the `greet` function
      const titles = result.map(l => l.command?.title);
      expect(titles).toContain("string => string");
    }));
});
