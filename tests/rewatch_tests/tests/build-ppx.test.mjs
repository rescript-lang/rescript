import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("ppx integration", () => {
  it("builds a package that uses a ppx", () =>
    runRewatchTest(async ({ createCli, fileExists }) => {
      const ppxCli = createCli("packages/with-ppx");
      const result = await ppxCli.build();
      expect(result.status).toBe(0);
      expect(fileExists("packages/with-ppx/src/User.mjs")).toBe(true);
    }));

  it("includes ppx flags in parser args", () =>
    runRewatchTest(async ({ createCli }) => {
      const ppxCli = createCli("packages/with-ppx");
      await ppxCli.build();
      const result = await ppxCli.compilerArgs("src/User.res");
      expect(result.status).toBe(0);
      const args = JSON.parse(result.stdout);
      expect(args.parser_args).toContain("-ppx");
      const ppxArg = args.parser_args.find(a => a.includes("noop-ppx"));
      expect(ppxArg).toBeDefined();
    }));

  it("does not include ppx flags in compiler args", () =>
    runRewatchTest(async ({ createCli }) => {
      const ppxCli = createCli("packages/with-ppx");
      await ppxCli.build();
      const result = await ppxCli.compilerArgs("src/User.res");
      expect(result.status).toBe(0);
      const args = JSON.parse(result.stdout);
      expect(args.compiler_args).not.toContain("-ppx");
    }));
});
