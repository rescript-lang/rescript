import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests that the daemon respects dev-dependency boundaries.
// A non-dev source file should not be able to import from a dev-dependency.
// A dev source file (in a directory marked type: "dev") can use dev-dependencies.
//
// Source: EXPERIMENT_DAEMON.md "Future Test Scenarios" - "Dev Dependency Boundary"

describe("dev-dependency-boundary", () => {
  it("non-dev source importing dev-dependency fails", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      const mainResPath = join(
        sandbox,
        "packages",
        "with-dev-deps",
        "src",
        "Main.res",
      );

      // Add import of Library (which is a dev-dependency) to non-dev source
      const original = await readFile(mainResPath, "utf8");
      await writeFile(mainResPath, original + "\nlet lib = Library.greeting\n");

      const result = await cli.build();

      // Build should fail because Library is a dev-dependency
      expect(result.status).not.toBe(0);
      expect(result.stdout + result.stderr).toContain("Library");
    }));

  it("dev source importing dev-dependency succeeds", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      const testResPath = join(
        sandbox,
        "packages",
        "with-dev-deps",
        "test",
        "Test.res",
      );

      // Add import of Library (dev-dependency) to dev source file
      const original = await readFile(testResPath, "utf8");
      await writeFile(testResPath, original + "\nlet lib = Library.greeting\n");

      const result = await cli.build();

      // Build should succeed because Test.res is in a dev source directory
      expect(result.status).toBe(0);
    }));
});
