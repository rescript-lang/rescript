import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp inlay hint", { timeout: 60_000 }, () => {
  it("returns inlay hints for value bindings in a file", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Library.res contains:
      //   let greeting = "hello from library"   (line 1)
      //   type user = {name: string}            (line 3)
      //   let admin: user = {name: "admin"}     (line 4)
      //   let greet = (name: string) => ...     (line 5)
      await lsp.openFile("packages/library/src/Library.res");
      const result = await lsp.inlayHintFor(
        "packages/library/src/Library.res",
        0,
        10,
      );
      expect(result).not.toBeNull();
      expect(Array.isArray(result)).toBe(true);

      // Should have type hints for `greeting` (: string) and `greet` (: string => string)
      const labels = result.map(h => h.label);
      expect(labels).toContain(": string");
    }));
});
