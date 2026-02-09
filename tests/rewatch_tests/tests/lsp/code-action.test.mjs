import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp code action", { timeout: 60_000 }, () => {
  it("returns code actions for a function definition", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      await lsp.openFile("packages/library/src/Library.res");

      // Request code actions on the `greet` function (line 5, 0-indexed)
      // let greet = (name: string) => "hello " ++ name
      const result = await lsp.codeActionFor(
        "packages/library/src/Library.res",
        5,
        4,
        5,
        44,
      );

      expect(result).not.toBeNull();
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThan(0);

      // Should include "Add Documentation template" for a function
      const titles = result.map(a => a.title);
      expect(titles).toContain("Add Documentation template");
    }));
});
