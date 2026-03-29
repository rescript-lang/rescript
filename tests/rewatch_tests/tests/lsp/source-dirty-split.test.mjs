import { writeFileSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp source-dirty split", { timeout: 60_000 }, () => {
  // Tests that the SourceImplementationDirty / SourceInterfaceDirty /
  // SourceBothDirty split works correctly for modules with .resi files.
  //
  // Dependency graph: Root → App → Library
  //
  // We add a Library.resi to constrain Library's public API,
  // then verify that impl-only and interface changes both compile correctly.

  it("handles implementation-only change on module with .resi", () =>
    runLspTest(async ({ lsp, sandbox, writeFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Add a .resi file for Library, exposing the existing API
      const libraryResi = path.join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.resi",
      );
      writeFileSync(
        libraryResi,
        "let greeting: string\ntype user = {name: string}\nlet admin: user\nlet greet: string => string\n",
      );

      // Notify LSP about the new file
      lsp.notifyWatchedFilesChanged([
        { relativePath: "packages/library/src/Library.resi", type: 1 },
      ]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Change only the implementation (body-only, same types)
      await writeFile(
        "packages/library/src/Library.res",
        '/***A library for greetings and users.*/\nlet greeting = "hello from library v2"\n\ntype user = {name: string}\nlet admin: user = {name: "admin"}\nlet greet = (name: string) => "hi " ++ name\n',
      );
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // No diagnostics — the API didn't change
      const diagnostics = lsp.getDiagnostics();
      for (const entry of diagnostics) {
        expect(
          entry.diagnostics,
          `Expected no diagnostics for ${entry.file}`,
        ).toEqual([]);
      }
    }));

  it("handles interface-only change that breaks dependents", () =>
    runLspTest(async ({ lsp, sandbox, writeFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Add a .resi file for Library
      const libraryResi = path.join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.resi",
      );
      writeFileSync(
        libraryResi,
        "let greeting: string\ntype user = {name: string}\nlet admin: user\nlet greet: string => string\n",
      );

      lsp.notifyWatchedFilesChanged([
        { relativePath: "packages/library/src/Library.resi", type: 1 },
      ]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Verify no errors after adding the .resi
      let diagnostics = lsp.getDiagnostics();
      for (const entry of diagnostics) {
        expect(
          entry.diagnostics,
          `Expected no diagnostics for ${entry.file} after adding .resi`,
        ).toEqual([]);
      }

      // Now change only the .resi to remove the greeting export
      await writeFile(
        "packages/library/src/Library.resi",
        "type user = {name: string}\nlet admin: user\nlet greet: string => string\n",
      );
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // App.res uses Library.greeting — should now have a diagnostic
      diagnostics = lsp.getDiagnostics();
      const appDiag = diagnostics.find(
        d => d.file === "packages/app/src/App.res",
      );
      expect(appDiag, "Expected diagnostics for App.res").toBeDefined();
      expect(appDiag.diagnostics.length).toBeGreaterThan(0);
      expect(appDiag.diagnostics[0].severity).toBe(1); // Error
    }));
});
