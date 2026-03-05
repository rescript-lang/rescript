import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("module operations", () => {
  it("cleans up old artifacts when a file is renamed", () =>
    runRewatchTest(
      async ({ cli, writeFileInSandbox, deleteFile, fileExists }) => {
        // Create a standalone file with no dependents
        await writeFileInSandbox(
          "packages/library/src/Helper.res",
          'let value = "hello"\n',
        );
        await cli.build();
        expect(fileExists("packages/library/src/Helper.mjs")).toBe(true);

        // Rename it
        await writeFileInSandbox(
          "packages/library/src/HelperRenamed.res",
          'let value = "hello"\n',
        );
        await deleteFile("packages/library/src/Helper.res");

        const result = await cli.build();
        expect(result.status).toBe(0);

        // Old artifact should be cleaned up, new one should exist
        expect(fileExists("packages/library/src/Helper.mjs")).toBe(false);
        expect(fileExists("packages/library/src/HelperRenamed.mjs")).toBe(true);
      },
    ));

  it("reports error when renaming a file that has dependents", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox, deleteFile }) => {
      await cli.build();

      // Rename Library.res -> LibRenamed.res, but App depends on Library
      await writeFileInSandbox(
        "packages/library/src/LibRenamed.res",
        'let greeting = "hello"\n',
      );
      await deleteFile("packages/library/src/Library.res");

      const result = await cli.build();
      expect(result.status).toBe(1);
      expect(result.stderr).toContain(
        "The module or file Library can't be found.",
      );
    }));

  it("detects duplicate module names", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      await writeFileInSandbox(
        "packages/library/src/nested/Library.res",
        "let value = 1\n",
      );

      const result = await cli.build();
      expect(result.status).toBe(1);
      expect(result.stderr).toContain("Duplicate module name: Library");
    }));
});

describe("interface files", () => {
  it("compiles a module with an interface file", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox, fileExists }) => {
      await writeFileInSandbox(
        "packages/library/src/Library.resi",
        "let greeting: string\n",
      );

      const result = await cli.build();
      expect(result.status).toBe(0);
      expect(fileExists("packages/library/src/Library.mjs")).toBe(true);
    }));

  it("reports error when interface does not match implementation", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      // Library.res has `let greeting = "hello"` (string)
      // Interface declares a different type
      await writeFileInSandbox(
        "packages/library/src/Library.resi",
        "let greeting: int\n",
      );

      const result = await cli.build();
      expect(result.status).toBe(1);
      expect(result.stderr).toContain("Failed to Compile");
    }));

  it("reports error when interface file has no implementation", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      await writeFileInSandbox(
        "packages/library/src/Orphan.resi",
        "let value: string\n",
      );

      const result = await cli.build();
      // This message is printed to stdout via println!
      expect(result.stdout).toContain(
        "No implementation file found for interface file",
      );
    }));

  it("reports error when implementation is renamed but interface remains", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox, deleteFile }) => {
      // First add an interface for Library
      await writeFileInSandbox(
        "packages/library/src/Library.resi",
        "let greeting: string\n",
      );
      await cli.build();

      // Rename the implementation, leaving the interface orphaned
      await writeFileInSandbox(
        "packages/library/src/LibRenamed.res",
        'let greeting = "hello"\n',
      );
      await deleteFile("packages/library/src/Library.res");

      const result = await cli.build();
      // Orphan interface message goes to stdout
      expect(result.stdout).toContain(
        "No implementation file found for interface file",
      );
    }));
});
