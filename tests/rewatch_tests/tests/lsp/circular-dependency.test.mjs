import { writeFileSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp circular dependency", { timeout: 60_000 }, () => {
  it("publishes diagnostics for a circular dependency between two files", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Create two files that import each other, forming a cycle:
      // CycleA -> CycleB -> CycleA
      const cycleAPath = path.join(sandbox, "src", "CycleA.res");
      const cycleBPath = path.join(sandbox, "src", "CycleB.res");
      writeFileSync(cycleAPath, "let a = CycleB.b\n");
      writeFileSync(cycleBPath, "let b = CycleA.a\n");

      lsp.notifyWatchedFilesChanged([
        { relativePath: "src/CycleA.res", type: 1 },
        { relativePath: "src/CycleB.res", type: 1 },
      ]);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      const diagnostics = lsp.getDiagnostics();

      // At least one of the cycle files should have a circular dependency diagnostic
      const cycleDiags = diagnostics.filter(
        d => d.file.includes("CycleA.res") || d.file.includes("CycleB.res"),
      );
      expect(cycleDiags.length).toBeGreaterThan(0);

      const hasCycleDiag = cycleDiags.some(d =>
        d.diagnostics.some(
          diag =>
            diag.severity === 1 && diag.message.includes("circular dependency"),
        ),
      );
      expect(
        hasCycleDiag,
        "Expected a circular dependency error diagnostic",
      ).toBe(true);
    }));
});
