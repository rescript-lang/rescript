import { existsSync, unlinkSync, writeFileSync } from "node:fs";
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

  it("picks up a newly created file after a Created event", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Create a new file with a type error on disk
      const newFilePath = path.join(sandbox, "src", "NewModule.res");
      writeFileSync(newFilePath, `let greeting: int = "hello"\n`);

      // Notify the LSP that a file was created (type: 1 = Created)
      lsp.notifyWatchedFilesChanged([
        { relativePath: "src/NewModule.res", type: 1 },
      ]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // The new module should have a type error diagnostic
      const diagnostics = lsp.getDiagnostics();
      const newModuleDiags = diagnostics.find(d =>
        d.file.includes("NewModule.res"),
      );
      expect(newModuleDiags).toBeDefined();
      expect(newModuleDiags.diagnostics.length).toBeGreaterThan(0);
      expect(newModuleDiags.diagnostics[0].message).toContain("string");
    }));

  it("clears diagnostics for a deleted file after a Deleted event", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // First, create a new module with a type error and let it build
      const newFilePath = path.join(sandbox, "src", "Temporary.res");
      writeFileSync(newFilePath, `let value: int = "oops"\n`);
      lsp.notifyWatchedFilesChanged([
        { relativePath: "src/Temporary.res", type: 1 },
      ]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Verify the module was picked up and has a type error diagnostic
      let diagnostics = lsp.getDiagnostics();
      const tempDiags = diagnostics.find(d => d.file.includes("Temporary.res"));
      expect(tempDiags).toBeDefined();
      expect(tempDiags.diagnostics.length).toBeGreaterThan(0);

      // Now delete the file and notify
      unlinkSync(newFilePath);
      lsp.notifyWatchedFilesChanged([
        { relativePath: "src/Temporary.res", type: 3 },
      ]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Diagnostics for the deleted file should be cleared (empty array)
      diagnostics = lsp.getDiagnostics();
      const temporaryDiags = diagnostics.find(d =>
        d.file.includes("Temporary.res"),
      );
      if (temporaryDiags) {
        expect(temporaryDiags.diagnostics).toEqual([]);
      }
    }));

  it("deleting a .res file also removes its .resi and compiled JS", () =>
    runLspTest(async ({ lsp, sandbox, writeFileExternal }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      const resPath = path.join(sandbox, "src", "Foo.res");
      const resiPath = path.join(sandbox, "src", "Foo.resi");
      const mjsPath = path.join(sandbox, "src", "Foo.mjs");

      // Create Foo.res and Foo.resi on disk
      writeFileSync(
        resPath,
        `let greet = (name: string) => "hello " ++ name\n`,
      );
      writeFileSync(resiPath, `let greet: string => string\n`);

      // Notify Created for both files → triggers full rebuild
      lsp.notifyWatchedFilesChanged([
        { relativePath: "src/Foo.res", type: 1 },
        { relativePath: "src/Foo.resi", type: 1 },
      ]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Trigger an incremental build to produce JS output
      await writeFileExternal(
        "src/Foo.res",
        `let greet = (name: string) => "hello " ++ name\n`,
      );
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Verify the JS output was produced
      expect(existsSync(mjsPath), "Foo.mjs should exist after build").toBe(
        true,
      );
      expect(existsSync(resiPath), "Foo.resi should exist").toBe(true);

      // Delete Foo.res and notify
      unlinkSync(resPath);
      lsp.notifyWatchedFilesChanged([{ relativePath: "src/Foo.res", type: 3 }]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // The .resi and .mjs should both be cleaned up
      expect(
        existsSync(resiPath),
        "Foo.resi should be deleted when Foo.res is deleted",
      ).toBe(false);
      expect(
        existsSync(mjsPath),
        "Foo.mjs should be deleted when Foo.res is deleted",
      ).toBe(false);
    }));

  it("deleting a .resi file does not remove the .res", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      const resPath = path.join(sandbox, "src", "Bar.res");
      const resiPath = path.join(sandbox, "src", "Bar.resi");

      // Create Bar.res and Bar.resi on disk
      writeFileSync(resPath, `let value = 42\n`);
      writeFileSync(resiPath, `let value: int\n`);

      // Notify Created → full rebuild
      lsp.notifyWatchedFilesChanged([
        { relativePath: "src/Bar.res", type: 1 },
        { relativePath: "src/Bar.resi", type: 1 },
      ]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Delete only the .resi and notify
      unlinkSync(resiPath);
      lsp.notifyWatchedFilesChanged([
        { relativePath: "src/Bar.resi", type: 3 },
      ]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // The .res should still exist — deleting .resi does not cascade
      expect(
        existsSync(resPath),
        "Bar.res should NOT be deleted when only Bar.resi is deleted",
      ).toBe(true);
    }));
});
