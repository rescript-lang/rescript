import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp document symbol", { timeout: 60_000 }, () => {
  it("returns document symbols for a file", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Library.res contains:
      //   let greeting = "hello from library"
      //   type user = {name: string}
      //   let admin: user = {name: "admin"}
      await lsp.openFile("packages/library/src/Library.res");
      const result = await lsp.documentSymbolsFor(
        "packages/library/src/Library.res",
      );
      expect(result).not.toBeNull();
      expect(Array.isArray(result)).toBe(true);

      const names = result.map(s => s.name);
      expect(names).toContain("greeting");
      expect(names).toContain("user");
      expect(names).toContain("admin");

      // The "user" type should have a child "name" (record field)
      const userSymbol = result.find(s => s.name === "user");
      expect(userSymbol.children).toBeDefined();
      const childNames = userSymbol.children.map(c => c.name);
      expect(childNames).toContain("name");
    }));
});
