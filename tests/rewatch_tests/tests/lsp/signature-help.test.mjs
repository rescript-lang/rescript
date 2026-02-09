import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp signatureHelp", { timeout: 60_000 }, () => {
  it("returns signature help for a function call", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const file = "packages/app/src/App.res";

      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      lsp.openFile(file);
      lsp.editFile(
        file,
        `let add = (a: int, b: int) => a + b
let x = add(1, 2)
`,
      );

      // Wait for didChange typecheck to produce an updated .cmt
      await lsp.waitForNotification("textDocument/publishDiagnostics", 10000);

      // Request signature help inside add(1, 2) — cursor on the second arg
      // Line 1, character 16 (on the "2")
      const result = await lsp.signatureHelpFor(file, 1, 16);
      expect(result).not.toBeNull();
      expect(result.signatures).toBeDefined();
      expect(result.signatures.length).toBeGreaterThan(0);
      expect(result.signatures[0].label).toContain("int");
    }));
});
