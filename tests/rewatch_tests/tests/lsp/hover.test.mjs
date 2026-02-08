import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp hover", { timeout: 60_000 }, () => {
  it("returns hover info for a let binding", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // The initial build already produced .cmt for Root.res
      // which contains: let main = App.run()
      await lsp.openFile("src/Root.res");
      // Hover over `main` at position (0, 4) — should show type `string`
      const result = await lsp.hoverFor("src/Root.res", 0, 4);
      expect(result).not.toBeNull();
      expect(result.contents.value).toContain("string");
    }));

  it("returns hover info for a module value access", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      await lsp.openFile("src/Root.res");
      // Hover over `run` in `App.run()` — at column 15
      const result = await lsp.hoverFor("src/Root.res", 0, 15);
      expect(result).not.toBeNull();
      expect(result.contents.value).toContain("string");
    }));
});
