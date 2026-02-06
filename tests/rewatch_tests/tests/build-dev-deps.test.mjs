import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("dev dependencies", () => {
  it("compiles dev source files with dev dependencies", () =>
    runRewatchTest(async ({ createCli, fileExists }) => {
      const devCli = createCli("packages/with-dev-deps");
      const result = await devCli.build();

      expect(result.status).toBe(0);
      // Regular source
      expect(fileExists("packages/with-dev-deps/src/Main.mjs")).toBe(true);
      // Dev source uses both Main and Library (dev dep)
      expect(fileExists("packages/with-dev-deps/test/Test.mjs")).toBe(true);
    }));

  it("reports error when non-dev source uses a dev dependency", () =>
    runRewatchTest(async ({ createCli, writeFileInSandbox }) => {
      // Make the regular source file use Library (which is a dev dep)
      await writeFileInSandbox(
        "packages/with-dev-deps/src/Main.res",
        "let greeting = Library.greeting\n",
      );

      const devCli = createCli("packages/with-dev-deps");
      const result = await devCli.build();

      expect(result.status).toBe(1);
      expect(result.stderr).toContain(
        "The module or file Library can't be found.",
      );
    }));

  it("cleans dev source artifacts", () =>
    runRewatchTest(async ({ createCli, fileExists }) => {
      const devCli = createCli("packages/with-dev-deps");
      await devCli.build();

      expect(fileExists("packages/with-dev-deps/src/Main.mjs")).toBe(true);
      expect(fileExists("packages/with-dev-deps/test/Test.mjs")).toBe(true);

      await devCli.clean();

      expect(fileExists("packages/with-dev-deps/src/Main.mjs")).toBe(false);
      expect(fileExists("packages/with-dev-deps/test/Test.mjs")).toBe(false);
    }));
});
