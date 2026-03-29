import { existsSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe(
  "rescript clean does not break running LSP",
  { timeout: 60_000 },
  () => {
    it("LSP continues to work after rescript clean", () =>
      runLspTest(async ({ lsp, sandbox, cli }) => {
        const rootUri = pathToFileURL(sandbox).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Save a file to trigger TypecheckAndEmit — produces JS + LSP artifacts
        lsp.saveFile("src/Root.res");
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        expect(
          existsSync(path.join(sandbox, "src", "Root.mjs")),
          "Root.mjs should exist after save",
        ).toBe(true);

        const lspDir = path.join(sandbox, "lib", "lsp");
        const lspOcamlDir = path.join(sandbox, "lib", "lsp-ocaml");
        expect(existsSync(lspDir), "lib/lsp/ should exist before clean").toBe(
          true,
        );
        expect(
          existsSync(lspOcamlDir),
          "lib/lsp-ocaml/ should exist before clean",
        ).toBe(true);

        // Run rescript clean (external CLI process)
        const cleanResult = await cli.clean();
        expect(cleanResult.status).toBe(0);

        // Verify clean did NOT delete LSP build directories
        expect(
          existsSync(lspDir),
          "lib/lsp/ should still exist after clean",
        ).toBe(true);
        expect(
          existsSync(lspOcamlDir),
          "lib/lsp-ocaml/ should still exist after clean",
        ).toBe(true);

        // Save a file again — LSP should still produce JS without errors
        lsp.saveFile("src/Root.res");
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        expect(
          existsSync(path.join(sandbox, "src", "Root.mjs")),
          "Root.mjs should exist after second save (post-clean)",
        ).toBe(true);

        // Diagnostics should be clean
        const diagnostics = lsp.getDiagnostics();
        for (const entry of diagnostics) {
          expect(entry.diagnostics).toEqual([]);
        }
      }));
  },
);
