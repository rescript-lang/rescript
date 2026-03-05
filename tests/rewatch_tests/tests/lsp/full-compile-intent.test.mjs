import { existsSync, readFileSync, writeFileSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp full-compile intent preservation", { timeout: 60_000 }, () => {
  // When a project rebuild reinitializes state (e.g. due to atomic file
  // writes from LLMs or git), modules that were previously at Built should
  // be restored via hash-based promotion — no bsc invocation needed.
  //
  // Scenario:
  // 1. Save Leaf.res → produces Leaf.mjs (now at Built).
  // 2. Write Leaf.res externally with the same content via delete+create
  //    (triggers project_build which reinitializes state).
  // 3. Verify Leaf.mjs still exists (hash promotion kept Built status).
  it("project rebuild preserves Built status via hash promotion", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile, deleteFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const leafMjs = path.join(lspCwd, "src", "Leaf.mjs");

        // Step 1: Save Leaf.res to produce JS (initial build is TypecheckOnly).
        await writeFile("src/Leaf.res", 'let value = "hello"\n');
        await lsp.waitForNotification("rescript/buildFinished", 30000);
        expect(existsSync(leafMjs), "Leaf.mjs should exist after save").toBe(
          true,
        );
        const originalContent = readFileSync(leafMjs, "utf8");

        // Step 2: Simulate atomic overwrite (delete+create) with same content.
        // This triggers project_build which reinitializes BuildCommandState.
        // Without hash promotion, Built modules are demoted to TypeChecked.
        await deleteFile("src/Leaf.res");
        const fullPath = path.join(lspCwd, "src", "Leaf.res");
        writeFileSync(fullPath, 'let value = "hello"\n');
        lsp.notifyWatchedFilesChanged([
          { relativePath: "src/Leaf.res", type: 3 }, // Deleted
          { relativePath: "src/Leaf.res", type: 1 }, // Created
        ]);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Leaf.mjs should still exist — hash promotion restored Built status
        // (or the intent set triggered recompilation).
        expect(
          existsSync(leafMjs),
          "Leaf.mjs must survive project rebuild (hash promotion or intent recompile)",
        ).toBe(true);
        const afterContent = readFileSync(leafMjs, "utf8");
        expect(afterContent).toEqual(originalContent);
      },
      { cwd: "packages/dep-chain" },
    ));

  // When a project rebuild occurs while a module has CompileError(FullCompile)
  // (user saved but compilation failed), the FullCompile intent must survive
  // the rebuild so the module gets JS when the error is eventually resolved.
  //
  // Scenario:
  // 1. Save App.res with a type error → CompileError(FullCompile).
  // 2. Create a new file on disk (Dummy.res) → triggers project_build.
  // 3. Save Button.res to fix the API → resolves App's error.
  // 4. Verify App.mjs exists (FullCompile intent survived the rebuild).
  it("project rebuild retries FullCompile for CompileError modules", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const appMjs = path.join(lspCwd, "src", "App.mjs");

        // Step 1: Save App.res with a type error.
        await writeFile(
          "src/App.res",
          'let run = () => (Leaf.value, Button.label("Hello"))\n',
        );
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // App.res should have diagnostics and no JS.
        let diagnostics = lsp.getDiagnostics();
        const appDiag = diagnostics.find(d => d.file === "src/App.res");
        expect(appDiag, "Expected diagnostics for App.res").toBeDefined();
        expect(appDiag.diagnostics.length).toBeGreaterThan(0);
        expect(
          existsSync(appMjs),
          "App.mjs should not exist after failed save",
        ).toBe(false);

        // Step 2: Create a new file to trigger project_build.
        // This reinitializes the BuildCommandState, potentially losing
        // the CompileError(FullCompile) state for App.
        const dummyPath = path.join(lspCwd, "src", "Dummy.res");
        writeFileSync(dummyPath, 'let dummy = "trigger rebuild"\n');
        lsp.notifyWatchedFilesChanged([
          { relativePath: "src/Dummy.res", type: 1 }, // Created
        ]);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Step 3: Fix Button.res API so App.res compiles.
        await writeFile(
          "src/Button.res",
          'let label = arg => "click me" ++ arg\n',
        );
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // All diagnostics should be cleared.
        diagnostics = lsp.getDiagnostics();
        for (const entry of diagnostics) {
          expect(
            entry.diagnostics,
            `Expected no diagnostics for ${entry.file}, got: ${JSON.stringify(entry.diagnostics)}`,
          ).toEqual([]);
        }

        // App.mjs MUST exist — the FullCompile intent survived the
        // project rebuild and was drained when Button.res was saved.
        expect(
          existsSync(appMjs),
          "App.mjs should exist — FullCompile intent must survive project rebuild",
        ).toBe(true);
      },
      { cwd: "packages/dep-chain" },
    ));

  // Post-build recheck (step 3 in flush) should NOT overwrite compile error
  // diagnostics with clean buffer typecheck results.
  //
  // Scenario:
  // 1. Open App.res in the editor (didOpen).
  // 2. Edit App.res with broken content (didChange) — buffer has bad content.
  // 3. Write the same broken content to disk externally (delete+create).
  //    This triggers both project_build (TypecheckOnly) and file_build
  //    (FullCompile), and the post-build recheck.
  // 4. Verify App.res has compile error diagnostics (not empty).
  it("post-build recheck does not overwrite compile errors", () =>
    runLspTest(
      async ({ lsp, lspCwd, deleteFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const brokenContent =
          'let run = () => (Leaf.value, Button.label("Hello"))\n';

        // Open App.res and edit with broken content.
        lsp.openFile("src/App.res");
        lsp.editFile("src/App.res", brokenContent);

        // Write the same broken content externally via delete+create.
        await deleteFile("src/App.res");
        const fullPath = path.join(lspCwd, "src", "App.res");
        writeFileSync(fullPath, brokenContent);
        lsp.notifyWatchedFilesChanged([
          { relativePath: "src/App.res", type: 3 }, // Deleted
          { relativePath: "src/App.res", type: 1 }, // Created
        ]);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // App.res should have compile error diagnostics.
        // Without the fix, the post-build recheck (which runs the buffer
        // through bsc -bs-read-stdin) might report no errors (if the file
        // typechecks clean) and overwrite the compile error diagnostics.
        const diagnostics = lsp.getDiagnostics();
        const appDiag = diagnostics.find(d => d.file === "src/App.res");
        expect(
          appDiag,
          "Expected diagnostics for App.res after broken external write",
        ).toBeDefined();
        expect(
          appDiag.diagnostics.length,
          "App.res should have compile error diagnostics, not be overwritten by recheck",
        ).toBeGreaterThan(0);
      },
      { cwd: "packages/dep-chain" },
    ));
});
