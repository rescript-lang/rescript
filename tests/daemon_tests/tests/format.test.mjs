import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests the Format command via the CLI. Format operates on source files in
// scope (determined by working directory). It supports formatting all project
// files, specific files, and checking without modifying.

describe("format", () => {
  it("formats project files from root", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);
      const result = await cli.format();
      expect(result.status).toBe(0);
    }));

  it("formats a specific file", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);
      const filePath = join(sandbox, "src", "Root.res");

      // Mess up formatting
      await writeFile(filePath, "let    main   =    App.run()\n");

      const result = await cli.format([filePath]);
      expect(result.status).toBe(0);

      // Verify the file was reformatted
      const content = await readFile(filePath, "utf8");
      expect(content.trim()).toBe("let main = App.run()");
    }));

  it("format --check exits 0 when files are formatted", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);
      const result = await cli.format(["--check"]);
      expect(result.status).toBe(0);
    }));

  it("format --check exits non-zero when files need formatting", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);
      const filePath = join(sandbox, "src", "Root.res");

      // Mess up formatting
      await writeFile(filePath, "let    main   =    App.run()\n");

      const result = await cli.format(["--check", filePath]);
      expect(result.status).not.toBe(0);
    }));

  it("formats stdin with --stdin .res", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);
      const result = await cli.formatStdin(".res", "let    x   =    1\n");
      expect(result.status).toBe(0);
      expect(result.stdout.trim()).toBe("let x = 1");
    }));

  it("formats from child package (scoped)", () =>
    runDaemonTest(async ({ sandbox }) => {
      const libDir = join(sandbox, "packages", "library");
      const cli = createRescriptCli(libDir);
      const result = await cli.format();
      expect(result.status).toBe(0);
    }));
});
