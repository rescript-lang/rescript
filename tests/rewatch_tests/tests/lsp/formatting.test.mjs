import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp formatting", { timeout: 60_000 }, () => {
  it("formats a ReScript file", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Open a file with poor formatting (extra spaces)
      const unformatted = "let    x   =    1\n";
      lsp.openFile("src/Root.res", unformatted);

      const edits = await lsp.formatFor("src/Root.res");
      expect(edits.length).toBe(1);
      expect(edits[0].newText).toBe("let x = 1\n");
    }));
});
