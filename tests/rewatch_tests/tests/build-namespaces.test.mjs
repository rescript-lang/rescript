import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("namespace packages", () => {
  it("builds a namespaced package", () =>
    runRewatchTest(async ({ createCli, fileExists }) => {
      const nsCli = createCli("packages/namespaced");
      const result = await nsCli.build();

      expect(result.status).toBe(0);
      expect(fileExists("packages/namespaced/src/Helper.mjs")).toBe(true);
      expect(fileExists("packages/namespaced/src/Consumer.mjs")).toBe(true);
    }));

  it("cleans up old artifacts when a file is renamed in a namespaced package", () =>
    runRewatchTest(
      async ({ createCli, writeFileInSandbox, deleteFile, fileExists }) => {
        const nsCli = createCli("packages/namespaced");
        await nsCli.build();
        expect(fileExists("packages/namespaced/src/Helper.mjs")).toBe(true);

        // Rename Helper.res -> HelperRenamed.res
        await writeFileInSandbox(
          "packages/namespaced/src/HelperRenamed.res",
          'let value = "hello from renamed"\n',
        );
        // Update Consumer to use the renamed module
        await writeFileInSandbox(
          "packages/namespaced/src/Consumer.res",
          "let message = HelperRenamed.value\n",
        );
        await deleteFile("packages/namespaced/src/Helper.res");

        const result = await nsCli.build();
        expect(result.status).toBe(0);

        expect(fileExists("packages/namespaced/src/Helper.mjs")).toBe(false);
        expect(fileExists("packages/namespaced/src/HelperRenamed.mjs")).toBe(
          true,
        );
      },
    ));
});
