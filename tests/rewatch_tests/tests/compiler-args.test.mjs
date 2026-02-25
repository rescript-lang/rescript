import path from "node:path";
import { describe, expect, it } from "vitest";
import { runtimePath } from "../helpers/bins.mjs";
import { runRewatchTest } from "../helpers/test-context.mjs";

/**
 * Normalize absolute paths and backslashes in compiler-args JSON output
 * so snapshots are stable across platforms.
 */
function normalizeCompilerArgs(json, sandboxPath) {
  const normalized = JSON.stringify(
    json,
    (_key, value) => {
      if (typeof value !== "string") return value;
      // Normalize backslashes to forward slashes
      let v = value.replace(/\\/g, "/");
      // Replace runtime path
      const normalizedRuntime = runtimePath.replace(/\\/g, "/");
      v = v.replace(normalizedRuntime, "<RUNTIME>");
      // Replace sandbox path
      const normalizedSandbox = sandboxPath.replace(/\\/g, "/");
      v = v.replace(normalizedSandbox, "<SANDBOX>");
      return v;
    },
    2,
  );
  return normalized;
}

describe("compiler-args", () => {
  it("returns compiler arguments for a source file", () =>
    runRewatchTest(async ({ cli, sandbox }) => {
      const filePath = path.join(sandbox, "packages/library/src/Library.res");
      const result = await cli.compilerArgs(filePath);
      const json = JSON.parse(result.stdout);
      expect(normalizeCompilerArgs(json, sandbox)).toMatchSnapshot();
    }));

  it("produces identical output regardless of working directory", () =>
    runRewatchTest(async ({ cli, sandbox, createCli }) => {
      const rootResult = await cli.compilerArgs(
        "packages/library/src/Library.res",
      );

      // Get compiler args from inside the library package
      const libCli = createCli("packages/library");
      const pkgResult = await libCli.compilerArgs("src/Library.res");

      expect(rootResult.status).toBe(0);
      expect(pkgResult.status).toBe(0);

      // Normalize both outputs and compare
      const rootJson = JSON.parse(rootResult.stdout);
      const pkgJson = JSON.parse(pkgResult.stdout);
      expect(normalizeCompilerArgs(rootJson, sandbox)).toBe(
        normalizeCompilerArgs(pkgJson, sandbox),
      );
    }));

  it("includes warning flags in both parser and compiler args", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox, readFileInSandbox }) => {
      // Patch the library package to add warning config
      const config = JSON.parse(
        await readFileInSandbox("packages/library/rescript.json"),
      );
      config.warnings = { number: "+8+27", error: "-27" };
      await writeFileInSandbox(
        "packages/library/rescript.json",
        JSON.stringify(config, null, 2) + "\n",
      );

      const result = await cli.compilerArgs("packages/library/src/Library.res");
      expect(result.status).toBe(0);

      const json = JSON.parse(result.stdout);

      // Warning flags should appear in parser_args
      expect(json.parser_args).toContain("-w");
      expect(json.parser_args).toContain("+8+27");
      expect(json.parser_args).toContain("-warn-error");
      expect(json.parser_args).toContain("-27");

      // Warning flags should also appear in compiler_args
      expect(json.compiler_args).toContain("-w");
      expect(json.compiler_args).toContain("+8+27");
      expect(json.compiler_args).toContain("-warn-error");
      expect(json.compiler_args).toContain("-27");
    }));
});
