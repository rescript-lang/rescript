import { existsSync, writeFileSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp config change", { timeout: 60_000 }, () => {
  it("saving rescript.json triggers a full rebuild that picks up new files", () =>
    runLspTest(async ({ lsp, sandbox, writeFile, readFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Create a new source file on disk without notifying the LSP.
      // The LSP doesn't know about it yet.
      const newFilePath = path.join(sandbox, "src", "FromConfig.res");
      writeFileSync(newFilePath, `let value: int = "not an int"\n`);

      // Save rescript.json (content unchanged) — this should trigger
      // a full project rebuild which re-scans sources and picks up
      // the new file.
      const config = await readFile("rescript.json");
      await writeFile("rescript.json", config);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // The new module should have been picked up and have a type error
      const diagnostics = lsp.getDiagnostics();
      const newDiag = diagnostics.find(d => d.file.includes("FromConfig.res"));
      expect(
        newDiag,
        "Expected diagnostics for FromConfig.res after config save",
      ).toBeDefined();
      expect(newDiag.diagnostics.length).toBeGreaterThan(0);
      expect(newDiag.diagnostics[0].severity).toBe(1); // Error
    }));

  it("changing suffix in rescript.json produces JS with the new extension", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const oldJs = path.join(lspCwd, "src", "CjsModule.bs.js");
        const newJs = path.join(lspCwd, "src", "CjsModule.js");

        // Initial build is typecheck-only, no JS output yet
        expect(existsSync(oldJs), "No .bs.js before save").toBe(false);
        expect(existsSync(newJs), "No .js before save").toBe(false);

        // Save a source file to produce JS with the original .bs.js suffix
        lsp.saveFile("src/CjsModule.res");
        await lsp.waitForNotification("rescript/buildFinished", 30000);
        expect(
          existsSync(oldJs),
          "CjsModule.bs.js should exist after save",
        ).toBe(true);

        // Change the suffix from .bs.js to .js in rescript.json
        const newConfig = JSON.stringify(
          {
            name: "@rewatch-test/commonjs",
            sources: { dir: "src", subdirs: true },
            "package-specs": { module: "commonjs", "in-source": true },
            suffix: ".js",
          },
          null,
          2,
        );
        await writeFile("rescript.json", newConfig);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // The full rebuild is typecheck-only, so save the source file
        // to trigger JS emission with the new suffix.
        lsp.saveFile("src/CjsModule.res");
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // The new .js file should exist
        expect(
          existsSync(newJs),
          "CjsModule.js should exist after suffix change and save",
        ).toBe(true);
      },
      { cwd: "packages/commonjs" },
    ));
});
