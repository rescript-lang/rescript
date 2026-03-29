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
});
