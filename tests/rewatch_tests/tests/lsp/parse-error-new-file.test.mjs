import { writeFileSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp parse error + new file creation", { timeout: 60_000 }, () => {
  it("preserves ParseError stage when creating a file triggers mark_modules_with_expired_deps", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Step 1: Break Leaf.res with a syntax error (parse error, not type error).
        await writeFile("src/Leaf.res", "let value = {\n");
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Leaf.res should have parse error diagnostics
        let diagnostics = lsp.getDiagnostics();
        const leafDiag = diagnostics.find(d => d.file === "src/Leaf.res");
        expect(leafDiag, "Expected diagnostics for Leaf.res").toBeDefined();
        expect(leafDiag.diagnostics.length).toBeGreaterThan(0);

        // Step 2: Create a new file that depends on Leaf (which has a parse error).
        const newFilePath = path.join(lspCwd, "src", "NewModule.res");
        writeFileSync(newFilePath, 'let greeting = "hello " ++ Leaf.value\n');
        lsp.notifyWatchedFilesChanged([
          { relativePath: "src/NewModule.res", type: 1 },
        ]);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Step 3: Fix Leaf.res
        await writeFile("src/Leaf.res", 'let value = "fixed"\n');
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Leaf.res diagnostics should be cleared
        diagnostics = lsp.getDiagnostics();
        const leafDiagAfter = diagnostics.find(d => d.file === "src/Leaf.res");
        if (leafDiagAfter) {
          expect(
            leafDiagAfter.diagnostics,
            `Expected no diagnostics for src/Leaf.res after fix, got: ${JSON.stringify(leafDiagAfter.diagnostics)}`,
          ).toEqual([]);
        }
      },
      { cwd: "packages/dep-chain" },
    ));
});
