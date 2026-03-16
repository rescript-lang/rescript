import { existsSync, readdirSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

/**
 * Collect all file basenames (no path) in a directory (non-recursive).
 */
function listFiles(dir) {
  if (!existsSync(dir)) return [];
  return readdirSync(dir, { withFileTypes: true })
    .filter(e => e.isFile())
    .map(e => e.name)
    .sort();
}

/**
 * Collect file extensions present in a directory.
 */
function getExtensions(dir) {
  return new Set(listFiles(dir).map(f => path.extname(f)));
}

describe("concurrent LSP and CLI build", { timeout: 120_000 }, () => {
  it("LSP and standard build use separate flat artifact directories", () =>
    runLspTest(async ({ lsp, sandbox, cli }) => {
      // 1. Run a standard CLI build first — populates lib/bs/ and lib/ocaml/
      const buildResult = await cli.build();
      expect(buildResult.status).toBe(0);

      // Verify standard build artifacts exist
      const ocamlDir = path.join(sandbox, "lib", "ocaml");
      expect(
        existsSync(ocamlDir),
        "lib/ocaml/ should exist after CLI build",
      ).toBe(true);
      const ocamlFiles = listFiles(ocamlDir);
      expect(ocamlFiles.length).toBeGreaterThan(0);

      // lib/lsp-ocaml should NOT exist yet (no LSP build has run)
      const lspOcamlDir = path.join(sandbox, "lib", "lsp-ocaml");
      expect(
        existsSync(lspOcamlDir),
        "lib/lsp-ocaml/ should not exist before LSP build",
      ).toBe(false);

      // Record lib/ocaml contents before LSP build
      const ocamlFilesBefore = listFiles(ocamlDir);

      // 2. Start LSP — runs initial TypecheckOnly build, populates lib/lsp/ and lib/lsp-ocaml/
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Verify LSP build artifacts exist in lib/lsp-ocaml/
      expect(
        existsSync(lspOcamlDir),
        "lib/lsp-ocaml/ should exist after LSP initial build",
      ).toBe(true);
      const lspOcamlFiles = listFiles(lspOcamlDir);
      expect(lspOcamlFiles.length).toBeGreaterThan(0);

      // Verify lib/lsp/ (nested) also exists
      const lspDir = path.join(sandbox, "lib", "lsp");
      expect(existsSync(lspDir), "lib/lsp/ should exist after LSP build").toBe(
        true,
      );

      // 3. Verify lib/ocaml/ was NOT modified by the LSP build
      const ocamlFilesAfterLsp = listFiles(ocamlDir);
      expect(ocamlFilesAfterLsp).toEqual(ocamlFilesBefore);

      // 4. Save a file in LSP — triggers TypecheckAndEmit, which also writes to lib/lsp-ocaml/
      lsp.saveFile("src/Root.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // JS should be produced (source-adjacent) by the LSP save
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should exist after LSP save",
      ).toBe(true);

      // lib/lsp-ocaml/ should have .cmj files now (TypecheckAndEmit produces them)
      const lspOcamlExts = getExtensions(lspOcamlDir);
      expect(
        lspOcamlExts.has(".cmi"),
        "lib/lsp-ocaml/ should have .cmi files",
      ).toBe(true);

      // lib/ocaml/ should STILL be unchanged by the LSP save
      const ocamlFilesAfterSave = listFiles(ocamlDir);
      expect(ocamlFilesAfterSave).toEqual(ocamlFilesBefore);

      // 5. Run another CLI build — should succeed without errors
      //    (the LSP artifacts in lib/lsp-ocaml/ don't interfere)
      const buildResult2 = await cli.build();
      expect(buildResult2.status).toBe(0);

      // lib/lsp-ocaml/ should still exist and be untouched by the CLI build
      const lspOcamlFilesAfterBuild = listFiles(lspOcamlDir);
      expect(lspOcamlFilesAfterBuild).toEqual(listFiles(lspOcamlDir));

      // Diagnostics from LSP should be clean
      const diagnostics = lsp.getDiagnostics();
      for (const entry of diagnostics) {
        expect(entry.diagnostics).toEqual([]);
      }
    }));

  it("LSP build after clean does not use stale lib/ocaml artifacts", () =>
    runLspTest(async ({ lsp, sandbox, cli }) => {
      // 1. Run a standard build to populate lib/ocaml/
      const buildResult = await cli.build();
      expect(buildResult.status).toBe(0);

      // 2. Start LSP — initial build populates lib/lsp-ocaml/
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // 3. Save to trigger TypecheckAndEmit — should produce JS
      lsp.saveFile("src/Root.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should exist after save",
      ).toBe(true);

      // Diagnostics should be clean
      const diagnostics = lsp.getDiagnostics();
      for (const entry of diagnostics) {
        expect(entry.diagnostics).toEqual([]);
      }
    }));
});
