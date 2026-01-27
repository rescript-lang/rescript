import { writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests build error scenarios via the daemon. When source files contain errors
// (type errors, syntax errors, missing modules), the build should fail
// gracefully: BuildFinished.success is false, and appropriate error events are
// emitted. The daemon stays alive and can build again after errors are fixed.

describe("build-errors", () => {
  it("build fails with a syntax error", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      // Introduce a syntax error
      const filePath = join(sandbox, "src", "Root.res");
      await writeFile(filePath, "let main = {\n");

      await build(sandbox);
    }));

  it("build fails with a type error", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      // Introduce a type error: calling a string as a function
      const filePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      await writeFile(filePath, 'let greeting = "hello"()\n');

      await build(sandbox);
    }));

  it("build recovers after fixing an error", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      const filePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );

      // First build: broken
      await writeFile(filePath, "let greeting = \n");
      await build(sandbox);

      // Second build: fixed
      await writeFile(filePath, 'let greeting = "hello"\n');
      await build(sandbox);
    }));

  it("build exits with non-zero status on error", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      // Introduce a syntax error
      const filePath = join(sandbox, "src", "Root.res");
      await writeFile(filePath, "let main = {\n");

      const result = await cli.build();
      expect(result.status).not.toBe(0);
    }));
});
