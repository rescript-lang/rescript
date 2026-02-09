import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp rename", { timeout: 60_000 }, () => {
  it("prepareRename returns range and placeholder", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Library.res line 1: let greeting = "hello from library"
      // Cursor on `greeting` at col 4
      await lsp.openFile("packages/library/src/Library.res");
      const result = await lsp.prepareRenameFor(
        "packages/library/src/Library.res",
        1,
        4,
      );
      expect(result).not.toBeNull();
      expect(result).toHaveProperty("range");
      expect(result).toHaveProperty("placeholder", "greeting");
    }));

  it("rename renames a value across the file", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Library.res line 1: let greeting = "hello from library"
      // Rename `greeting` to `hello`
      await lsp.openFile("packages/library/src/Library.res");
      const result = await lsp.renameFor(
        "packages/library/src/Library.res",
        1,
        4,
        "hello",
      );
      expect(result).not.toBeNull();
      expect(result).toHaveProperty("documentChanges");
      expect(Array.isArray(result.documentChanges)).toBe(true);
      expect(result.documentChanges.length).toBeGreaterThan(0);

      // Check that at least one edit contains the new name
      const hasNewName = result.documentChanges.some(dc =>
        dc.edits?.some(edit => edit.newText === "hello"),
      );
      expect(hasNewName).toBe(true);
    }));
});
