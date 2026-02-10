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
});
