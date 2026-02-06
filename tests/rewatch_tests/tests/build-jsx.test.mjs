import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("jsx", () => {
  it("builds a package with JSX components", () =>
    runRewatchTest(async ({ createCli, fileExists }) => {
      const jsxCli = createCli("packages/with-jsx");
      const result = await jsxCli.build();
      expect(result.status).toBe(0);
      expect(fileExists("packages/with-jsx/src/Greeting.mjs")).toBe(true);
    }));

  it("includes jsx flags in parser args", () =>
    runRewatchTest(async ({ createCli }) => {
      const jsxCli = createCli("packages/with-jsx");
      await jsxCli.build();
      const result = await jsxCli.compilerArgs("src/Greeting.res");
      expect(result.status).toBe(0);
      const args = JSON.parse(result.stdout);
      expect(args.parser_args).toContain("-bs-jsx");
      expect(args.parser_args).toContain("4");
    }));

  it("includes jsx preserve flag when enabled", () =>
    runRewatchTest(
      async ({ createCli, readFileInSandbox, writeFileInSandbox }) => {
        const jsxCli = createCli("packages/with-jsx");
        const config = JSON.parse(
          await readFileInSandbox("packages/with-jsx/rescript.json"),
        );
        config.jsx.preserve = true;
        await writeFileInSandbox(
          "packages/with-jsx/rescript.json",
          JSON.stringify(config, null, 2) + "\n",
        );
        await jsxCli.build();
        const result = await jsxCli.compilerArgs("src/Greeting.res");
        expect(result.status).toBe(0);
        const args = JSON.parse(result.stdout);
        expect(args.parser_args).toContain("-bs-jsx-preserve");
      },
    ));
});
