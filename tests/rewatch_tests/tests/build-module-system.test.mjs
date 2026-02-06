import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("module system and suffix", () => {
  it("builds a CommonJS package with .bs.js suffix", () =>
    runRewatchTest(async ({ createCli, fileExists }) => {
      const cjsCli = createCli("packages/commonjs");
      const result = await cjsCli.build();
      expect(result.status).toBe(0);
      expect(fileExists("packages/commonjs/src/CjsModule.bs.js")).toBe(true);
    }));

  it("includes commonjs module in compiler args", () =>
    runRewatchTest(async ({ createCli }) => {
      const cjsCli = createCli("packages/commonjs");
      await cjsCli.build();
      const result = await cjsCli.compilerArgs("src/CjsModule.res");
      expect(result.status).toBe(0);
      expect(result.stdout).toContain("-bs-package-output");
      expect(result.stdout).toContain("commonjs");
      expect(result.stdout).toContain(".bs.js");
    }));

  it("rebuilds with new suffix when config changes", () =>
    runRewatchTest(
      async ({
        createCli,
        readFileInSandbox,
        writeFileInSandbox,
        fileExists,
      }) => {
        const cjsCli = createCli("packages/commonjs");
        await cjsCli.build();
        expect(fileExists("packages/commonjs/src/CjsModule.bs.js")).toBe(true);

        const config = JSON.parse(
          await readFileInSandbox("packages/commonjs/rescript.json"),
        );
        config.suffix = ".res.js";
        await writeFileInSandbox(
          "packages/commonjs/rescript.json",
          JSON.stringify(config, null, 2) + "\n",
        );

        await cjsCli.build();
        expect(fileExists("packages/commonjs/src/CjsModule.res.js")).toBe(true);
      },
    ));

  it("reports error for duplicate package-spec suffixes", () =>
    runRewatchTest(async ({ cli, readFileInSandbox, writeFileInSandbox }) => {
      const config = JSON.parse(await readFileInSandbox("rescript.json"));
      config["package-specs"] = [
        { module: "commonjs", "in-source": true, suffix: ".js" },
        { module: "esmodule", "in-source": true, suffix: ".js" },
      ];
      await writeFileInSandbox(
        "rescript.json",
        JSON.stringify(config, null, 2) + "\n",
      );

      const result = await cli.build();
      expect(result.status).toBe(1);
      expect(result.stderr).toContain("Duplicate package-spec suffix");
    }));
});
