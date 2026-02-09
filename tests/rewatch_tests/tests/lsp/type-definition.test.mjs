import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp type definition", { timeout: 60_000 }, () => {
  it("jumps to type definition of a value", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Library.res line 4: let admin: user = {name: "admin"}
      // admin has type user, defined at line 3 of Library.res
      await lsp.openFile("packages/library/src/Library.res");
      const result = await lsp.typeDefinitionFor(
        "packages/library/src/Library.res",
        4,
        4,
      );
      expect(result).not.toBeNull();
      expect(result.uri).toContain("Library.res");
      expect(result.range.start.line).toBe(3);
    }));
});
