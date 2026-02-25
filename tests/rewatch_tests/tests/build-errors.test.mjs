import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("build errors", () => {
  it("reports parse errors", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      await writeFileInSandbox(
        "packages/library/src/Library.res",
        'let greeting = "unterminated\n',
      );

      const result = await cli.build();

      expect(result.status).toBe(1);
      expect(result.stderr).toContain("Syntax error!");
      expect(result.stderr).toContain(
        "This string is missing a double quote at the end",
      );
      expect(result.stderr).toContain("Could not parse Source Files");
    }));

  it("reports type errors", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      await writeFileInSandbox(
        "packages/library/src/Library.res",
        "let greeting = DoesNotExist.value\n",
      );

      const result = await cli.build();

      expect(result.status).toBe(1);
      expect(result.stderr).toContain(
        "The module or file DoesNotExist can't be found.",
      );
      expect(result.stderr).toContain("Failed to Compile");
    }));

  it("reports errors when a dependency is deleted", () =>
    runRewatchTest(async ({ cli, deleteFile }) => {
      // App depends on Library — build first, then delete Library
      await cli.build();
      await deleteFile("packages/library/src/Library.res");

      const result = await cli.build();

      expect(result.status).toBe(1);
      expect(result.stderr).toContain(
        "The module or file Library can't be found.",
      );
      expect(result.stderr).toContain("Failed to Compile");
    }));

  it("detects circular dependencies", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      // Create two files in the same package that import each other
      await writeFileInSandbox(
        "packages/library/src/Library.res",
        "let greeting = LibHelper.value\n",
      );
      await writeFileInSandbox(
        "packages/library/src/LibHelper.res",
        "let value = Library.greeting\n",
      );

      const result = await cli.build();

      expect(result.status).toBe(1);
      expect(result.stderr).toContain("Found a circular dependency");
      expect(result.stderr).toContain("LibHelper");
      expect(result.stderr).toContain("Library");
    }));
});
