import { describe, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("build", () => {
  it("builds all packages from root", () =>
    runRewatchTest(async ({ cli }) => {
      await cli.build();
    }));
});
