import { existsSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp didChangeWatchedFiles", { timeout: 60_000 }, () => {
  it("triggers a build when an external file change is reported", () =>
    runLspTest(async ({ lsp, sandbox, writeFileExternal }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // No JS after initial type-check-only build
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "No .mjs should exist after initial build",
      ).toBe(false);

      // Simulate an external edit (git checkout, terminal, LLM agent, etc.)
      await writeFileExternal("src/Root.res", `let main = App.run()\n`);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // JS should now be produced
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should exist after external change",
      ).toBe(true);

      // Diagnostics should be clean
      const diagnostics = lsp.getDiagnostics();
      for (const entry of diagnostics) {
        expect(entry.diagnostics).toEqual([]);
      }
    }));
});
