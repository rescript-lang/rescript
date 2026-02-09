import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

describe("lsp multi-workspace", { timeout: 60_000 }, () => {
  it("provides hover for an independent package not in root dependencies", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // CjsModule.res is in packages/commonjs — an independent project
      // not referenced by the root rescript.json's dependencies.
      // Multi-project discovery should find it and build it.
      await lsp.openFile("packages/commonjs/src/CjsModule.res");
      const result = await lsp.hoverFor(
        "packages/commonjs/src/CjsModule.res",
        0,
        4,
      );
      expect(result).not.toBeNull();
      expect(result.contents.value).toContain("string");
    }));

  it("provides hover for both root monorepo and independent package", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);
      await lsp.waitForNotification("rescript/buildFinished", 30000);

      // Hover on a monorepo file (Root.res depends on App from @rewatch-test/app)
      await lsp.openFile("src/Root.res");
      const rootResult = await lsp.hoverFor("src/Root.res", 0, 4);
      expect(rootResult).not.toBeNull();
      expect(rootResult.contents.value).toContain("string");

      // Hover on an independent package file
      await lsp.openFile("packages/commonjs/src/CjsModule.res");
      const cjsResult = await lsp.hoverFor(
        "packages/commonjs/src/CjsModule.res",
        0,
        4,
      );
      expect(cjsResult).not.toBeNull();
      expect(cjsResult.contents.value).toContain("string");
    }));
});
