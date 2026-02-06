import { describe, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("clean", () => {
  it("cleans build artifacts", () =>
    runRewatchTest(async ({ cli }) => {
      // First build, then clean
      await cli.build();
      await cli.clean();
    }));
});
