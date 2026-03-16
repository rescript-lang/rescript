import { existsSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe(
  "lsp initial save compiles dependencies without cmj",
  { timeout: 60_000 },
  () => {
    // Regression test: after the initial TypecheckOnly build the LSP produces
    // .cmi/.cmt but NO .cmj files. When cleanup_previous_build determines
    // whether a module is already fully compiled it must check for .cmj
    // existence — not just .cmt freshness. Without the fix, modules were
    // incorrectly marked as Built (despite missing .cmj), so the first
    // didSave would skip compiling dependencies and fail with missing-module
    // errors.
    //
    // Dependency graph (dep-chain): Main → App → (Leaf, Button)
    //
    // Scenario:
    // 1. Initial build (TypecheckOnly) — no .cmj files produced.
    // 2. Save Main.res — compile_dependencies should compile the full
    //    closure (Leaf, Button, App, Main) because none have .cmj yet.
    //
    // BUG: cleanup_previous_build saw a fresh .cmt and marked modules as
    // Built. The save then skipped compiling App/Leaf/Button (already
    // "Built"), but Main's compilation needs App.cmj → error.
    //
    // FIX: read_compile_state now tracks .cmj files. cleanup_previous_build
    // marks modules as TypeChecked (not Built) when .cmj is missing.
    it("compiles the full dependency closure on first save after initial build", () =>
      runLspTest(
        async ({ lsp, lspCwd }) => {
          const rootUri = pathToFileURL(lspCwd).href;
          await lsp.initialize(rootUri);
          await lsp.waitForNotification("rescript/buildFinished", 30000);

          // After initial TypecheckOnly build, no .mjs files should exist
          expect(
            existsSync(path.join(lspCwd, "src", "Main.mjs")),
            "Main.mjs should not exist after initial build",
          ).toBe(false);
          expect(
            existsSync(path.join(lspCwd, "src", "App.mjs")),
            "App.mjs should not exist after initial build",
          ).toBe(false);

          // Save Main.res — triggers TypecheckAndEmit for Main and its
          // full dependency closure (App, Leaf, Button).
          lsp.saveFile("src/Main.res");
          await lsp.waitForNotification("rescript/buildFinished", 30000);

          // All modules in the dependency closure should have JS output
          expect(
            existsSync(path.join(lspCwd, "src", "Main.mjs")),
            "Main.mjs should exist after save",
          ).toBe(true);
          expect(
            existsSync(path.join(lspCwd, "src", "App.mjs")),
            "App.mjs should exist after save (dependency of Main)",
          ).toBe(true);
          expect(
            existsSync(path.join(lspCwd, "src", "Leaf.mjs")),
            "Leaf.mjs should exist after save (transitive dependency)",
          ).toBe(true);
          expect(
            existsSync(path.join(lspCwd, "src", "Button.mjs")),
            "Button.mjs should exist after save (transitive dependency)",
          ).toBe(true);

          // No diagnostics — clean compilation
          const diagnostics = lsp.getDiagnostics();
          for (const entry of diagnostics) {
            expect(
              entry.diagnostics,
              `Expected no diagnostics for ${entry.file}, got: ${JSON.stringify(entry.diagnostics)}`,
            ).toEqual([]);
          }
        },
        { cwd: "packages/dep-chain" },
      ));
  },
);
