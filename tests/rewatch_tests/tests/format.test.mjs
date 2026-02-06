import { describe, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("format", () => {
  it("does not rewrite already formatted files", () =>
    runRewatchTest(async ({ cli }) => {
      await cli.format();
    }));
});
