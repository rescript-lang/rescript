import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp completion", { timeout: 60_000 }, () => {
  it("returns completions for a module dot access", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Type "App." to trigger module completion on an unsaved buffer
      lsp.editFile("src/Root.res", "let x = App.\n");

      // Wait for didChange typecheck to complete (produces .cmt)
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      const items = await lsp.completeFor("src/Root.res", 0, 12);
      const labels = items.map(i => i.label);
      // App module exports `run`
      expect(labels).toContain("run");
    }));
});
