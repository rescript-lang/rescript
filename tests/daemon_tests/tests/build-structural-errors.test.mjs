import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { runDaemonTest } from "../helpers/test-context-v2.mjs";

// Tests structural build errors: dependency cycles and duplicate modules.
// These correspond to bash tests compile/09 and compile/10 in rewatch/tests.

describe("build-structural-errors", () => {
  it("build fails with a dependency cycle", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      // Create a cycle: Library depends on App, but App already depends on Library
      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await writeFile(libraryPath, "let greeting = App.run()\n");

      await build(sandbox);
    }));

  it("build fails with a dependency cycle (non-zero exit)", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      const libraryPath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await writeFile(libraryPath, "let greeting = App.run()\n");

      const result = await cli.build();
      expect(result.status).not.toBe(0);
    }));

  it("build fails with duplicate module names", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      // Create two files that define the same module in the same package
      const dupeDir = join(sandbox, "packages", "library", "src", "subdir");
      await mkdir(dupeDir, { recursive: true });
      await writeFile(
        join(dupeDir, "Library.res"),
        'let duplicate = "I am a duplicate"\n',
      );

      await build(sandbox);
    }));

  it("build fails with duplicate module names (non-zero exit)", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      const dupeDir = join(sandbox, "packages", "library", "src", "subdir");
      await mkdir(dupeDir, { recursive: true });
      await writeFile(
        join(dupeDir, "Library.res"),
        'let duplicate = "I am a duplicate"\n',
      );

      const result = await cli.build();
      expect(result.status).not.toBe(0);
    }));
});
