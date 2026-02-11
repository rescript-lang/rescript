import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp", { timeout: 120_000 }, () => {
  it("initializes and shuts down cleanly", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      const result = await lsp.initialize(rootUri);

      expect(result.serverInfo).toEqual({
        name: "rescript-lsp",
      });
    }));
});
