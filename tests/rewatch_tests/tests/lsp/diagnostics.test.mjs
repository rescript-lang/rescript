import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp diagnostics", () => {
  it(
    "publishes no diagnostics for a clean build",
    () =>
      runLspTest(async ({ lsp, sandbox }) => {
        const rootUri = pathToFileURL(sandbox).href;
        await lsp.initialize(rootUri);

        await lsp.waitForNotification("rescript/buildFinished", 15000);
        const diagnostics = lsp.getDiagnostics();

        // No diagnostics published — editor starts with a clean slate
        expect(diagnostics).toEqual([]);
      }),
    45_000,
  );

  it(
    "publishes parse error diagnostics from initial build",
    () =>
      runLspTest(async ({ lsp, sandbox, writeFile }) => {
        // Introduce a syntax error before the LSP starts building
        await writeFile("src/Root.res", "let main = \n");

        const rootUri = pathToFileURL(sandbox).href;
        await lsp.initialize(rootUri);

        // Wait for the server to finish the initial build
        await lsp.waitForNotification("rescript/buildFinished", 30000);
        const diagnostics = lsp.getDiagnostics();

        // Find diagnostics for Root.res
        const rootDiag = diagnostics.find(d => d.file === "src/Root.res");
        expect(rootDiag, "Expected diagnostics for src/Root.res").toBeDefined();
        expect(rootDiag.diagnostics.length).toBeGreaterThan(0);

        const diag = rootDiag.diagnostics[0];
        expect(diag.severity).toBe(1); // Error
        expect(diag.message).toContain("This let-binding misses an expression");
      }),
    45_000,
  );

  it(
    "publishes type error diagnostics from initial build",
    () =>
      runLspTest(async ({ lsp, sandbox, writeFile }) => {
        // Introduce a type error: App.run() returns string, not int
        await writeFile("src/Root.res", "let main: int = App.run()\n");

        const rootUri = pathToFileURL(sandbox).href;
        await lsp.initialize(rootUri);

        await lsp.waitForNotification("rescript/buildFinished", 30000);
        const diagnostics = lsp.getDiagnostics();

        const rootDiag = diagnostics.find(d => d.file === "src/Root.res");
        expect(rootDiag, "Expected diagnostics for src/Root.res").toBeDefined();
        expect(rootDiag.diagnostics.length).toBeGreaterThan(0);

        const diag = rootDiag.diagnostics[0];
        expect(diag.severity).toBe(1); // Error
        expect(diag.message).toContain("This has type: string");
        expect(diag.message).toContain("But it's expected to have type: int");
      }),
    45_000,
  );
});
