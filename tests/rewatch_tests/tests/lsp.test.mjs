import { describe, expect, it } from "vitest";
import { runLspTest } from "../helpers/test-context.mjs";

describe("lsp", () => {
  it("initializes and shuts down cleanly", () =>
    runLspTest(async ({ lsp }) => {
      const result = await lsp.initialize(`file:///tmp`);

      expect(result.serverInfo).toEqual({
        name: "rescript-lsp",
      });
    }));
});
