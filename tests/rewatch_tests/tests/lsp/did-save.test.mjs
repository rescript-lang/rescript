import { existsSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp didSave", { timeout: 60_000 }, () => {
  it("produces JS output when a file is saved", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);

      // Wait for initial type-check-only build
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Verify no JS files exist after initial build (TypecheckOnly profile)
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "No .mjs should exist after initial build",
      ).toBe(false);

      // Save the file without changes — triggers TypecheckAndEmit build
      lsp.saveFile("src/Root.res");

      // Wait for the incremental build to finish
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Now JS files should exist for the saved file and its dependencies
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should exist after save",
      ).toBe(true);

      // Diagnostics should be clean
      const diagnostics = lsp.getDiagnostics();
      expect(diagnostics).toEqual([]);
    }));

  it("produces JS output for dependent files when a file is saved", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      const appMjs = path.join(sandbox, "packages", "app", "src", "App.mjs");
      const libraryMjs = path.join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.mjs",
      );

      // No JS in dependency packages after initial type-check-only build
      expect(existsSync(appMjs), "App.mjs should not exist before save").toBe(
        false,
      );
      expect(
        existsSync(libraryMjs),
        "Library.mjs should not exist before save",
      ).toBe(false);

      // Save Root.res — its dependency closure spans across packages:
      // Root (rewatch-test-fixture) → App (@rewatch-test/app) → Library (@rewatch-test/library)
      lsp.saveFile("src/Root.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // JS should be produced in each dependency package
      expect(existsSync(appMjs), "App.mjs should exist after save").toBe(true);
      expect(
        existsSync(libraryMjs),
        "Library.mjs should exist after save",
      ).toBe(true);
    }));

  it("does not compile unrelated files when a file is saved", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Save Unrelated.res — it has no dependencies, so only itself
      // should be compiled. Root, App, Library should be excluded.
      lsp.saveFile("packages/library/src/Unrelated.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Unrelated.mjs should exist (it was saved)
      expect(
        existsSync(
          path.join(sandbox, "packages", "library", "src", "Unrelated.mjs"),
        ),
        "Unrelated.mjs should exist after save",
      ).toBe(true);

      // Root.res is not in Unrelated's dependency closure —
      // it should NOT be compiled to JS
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should not exist (not in dependency closure)",
      ).toBe(false);
    }));

  it("preserves JS output from previous saves when saving an unrelated file", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // First save Root.res — compiles Root + its dependency closure
      lsp.saveFile("src/Root.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should exist after first save",
      ).toBe(true);

      // Now save Unrelated.res — Root is outside its closure but was
      // already Built. Its JS output and compilation stage must survive.
      lsp.saveFile("packages/library/src/Unrelated.res");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Root.mjs should still exist from the previous save
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should still exist after saving an unrelated file",
      ).toBe(true);
      expect(
        existsSync(
          path.join(sandbox, "packages", "library", "src", "Unrelated.mjs"),
        ),
        "Unrelated.mjs should exist after save",
      ).toBe(true);
    }));

  it("publishes diagnostics when saving a file with an error", () =>
    runLspTest(async ({ lsp, sandbox, writeFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Introduce a type error
      await writeFile("src/Root.res", "let main: int = App.run()\n");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Should have diagnostics for the error
      const diagnostics = lsp.getDiagnostics();
      const rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      expect(rootDiag, "Expected diagnostics for Root.res").toBeDefined();
      expect(rootDiag.diagnostics.length).toBeGreaterThan(0);
      expect(rootDiag.diagnostics[0].severity).toBe(1); // Error
    }));

  it("compiles files from external npm packages in the dependency closure", () =>
    runLspTest(
      async ({ lsp, sandbox, lspCwd }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        const bunPkg = path.join(sandbox, "node_modules", "rescript-bun");
        const bunCmj = path.join(
          bunPkg,
          "lib",
          "lsp",
          "src",
          "Bun-RescriptBun.cmj",
        );

        // Initial type-check-only build does not produce .cmj files
        expect(
          existsSync(bunCmj),
          "Bun .cmj should not exist after initial build",
        ).toBe(false);

        // Save UsesBun.res — it imports RescriptBun.Bun.version,
        // so the dependency closure spans into the rescript-bun npm package
        lsp.saveFile("src/UsesBun.res");
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // After save, TypecheckAndEmit produces .cmj in lib/lsp
        expect(existsSync(bunCmj), "Bun .cmj should exist after save").toBe(
          true,
        );
      },
      { cwd: "packages/with-deps" },
    ));

  it("does not recompile npm package modules on subsequent saves", () =>
    runLspTest(
      async ({ lsp, lspCwd, writeFile }) => {
        const rootUri = pathToFileURL(lspCwd).href;
        await lsp.initialize(rootUri);
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // First save compiles UsesBun + the entire rescript-bun namespace
        lsp.saveFile("src/UsesBun.res");
        await lsp.waitForNotification("rescript/buildFinished", 30000);

        // Second save — rescript-bun modules are already at Built,
        // so only UsesBun should be recompiled
        await writeFile(
          "src/UsesBun.res",
          "let version = RescriptBun.Bun.version\nlet x = 1\n",
        );
        await lsp.waitForNotification("rescript/buildFinished", 30000);
      },
      { cwd: "packages/with-deps" },
    ));

  it("clears diagnostics when error is fixed and file is saved", () =>
    runLspTest(async ({ lsp, sandbox, writeFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Introduce a parse error
      await writeFile("src/Root.res", "let main = \n");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Should have diagnostics
      let diagnostics = lsp.getDiagnostics();
      let rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      expect(rootDiag, "Expected diagnostics for parse error").toBeDefined();
      expect(rootDiag.diagnostics.length).toBeGreaterThan(0);

      // Fix the error
      await writeFile("src/Root.res", "let main = App.run()\n");
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Diagnostics should be cleared
      diagnostics = lsp.getDiagnostics();
      rootDiag = diagnostics.find(d => d.file === "src/Root.res");
      if (rootDiag) {
        expect(rootDiag.diagnostics).toEqual([]);
      }

      // JS should exist after fixing
      expect(
        existsSync(path.join(sandbox, "src", "Root.mjs")),
        "Root.mjs should exist after fix",
      ).toBe(true);
    }));

  it("publishes diagnostics for dependent files when a used API changes on save", () =>
    runLspTest(async ({ lsp, sandbox, writeFile }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Break the API: rename greeting → greetingRenamed in Library.res.
      // App.res uses Library.greeting, so saving Library.res should
      // typecheck its dependents and surface the error in App.res —
      // without needing to save Root.res or App.res.
      await writeFile(
        "packages/library/src/Library.res",
        'let greetingRenamed = "hello from library"\n',
      );
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // App.res should have diagnostics (unbound value Library.greeting)
      const diagnostics = lsp.getDiagnostics();
      const appDiag = diagnostics.find(
        d => d.file === "packages/app/src/App.res",
      );
      expect(appDiag, "Expected diagnostics for App.res").toBeDefined();
      expect(appDiag.diagnostics.length).toBeGreaterThan(0);
      expect(appDiag.diagnostics[0].severity).toBe(1); // Error
    }));
});
