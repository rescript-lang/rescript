import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests the GetCompilerArgs RPC. The daemon creates a ProjectContext at startup
// (parsing root rescript.json + resolving monorepo context), but does NOT load
// packages or sources until a build/watch/format request triggers setup_client().
// GetCompilerArgs bypasses setup_client entirely — it reads the file's package
// config from disk and uses the pre-existing ProjectContext. This means it works
// immediately after daemon start, regardless of which packages have been loaded.

describe("compiler-args", () => {
  it("returns compiler args without a prior build", () =>
    runDaemonTest(async ({ sandbox, grpcClient }) => {
      const filePath = join(sandbox, "src", "Root.res");
      const response = await grpcClient.GetCompilerArgs({
        file_path: filePath,
      });

      expect(response.compiler_args).toBeDefined();
      expect(response.compiler_args.length).toBeGreaterThan(0);
      expect(response.parser_args).toBeDefined();
      expect(response.parser_args.length).toBeGreaterThan(0);
    }));

  it("returns compiler args for a file in a child package", () =>
    runDaemonTest(async ({ sandbox, grpcClient }) => {
      const filePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      const response = await grpcClient.GetCompilerArgs({
        file_path: filePath,
      });

      expect(response.compiler_args).toBeDefined();
      expect(response.compiler_args.length).toBeGreaterThan(0);
      expect(response.parser_args).toBeDefined();
      expect(response.parser_args.length).toBeGreaterThan(0);
    }));

  it("includes warning flags in both parser_args and compiler_args", () =>
    runDaemonTest(async ({ sandbox, grpcClient }) => {
      // The with-warnings package has warnings.number = "+1000" and warnings.error = "-2000"
      const filePath = join(
        sandbox,
        "packages",
        "with-warnings",
        "src",
        "Sample.res",
      );
      const response = await grpcClient.GetCompilerArgs({
        file_path: filePath,
      });

      expect(response.compiler_args).toBeDefined();
      expect(response.parser_args).toBeDefined();

      // Check that warning flags appear in parser_args
      const parserArgsStr = response.parser_args.join(" ");
      expect(parserArgsStr).toContain("-w");
      expect(parserArgsStr).toContain("+1000");
      expect(parserArgsStr).toContain("-warn-error");
      expect(parserArgsStr).toContain("-2000");

      // Check that warning flags appear in compiler_args
      const compilerArgsStr = response.compiler_args.join(" ");
      expect(compilerArgsStr).toContain("-w");
      expect(compilerArgsStr).toContain("+1000");
      expect(compilerArgsStr).toContain("-warn-error");
      expect(compilerArgsStr).toContain("-2000");
    }));

  it("returns identical output regardless of working directory", () =>
    runDaemonTest(async ({ sandbox }) => {
      // CLI from root with absolute path
      const cliFromRoot = createRescriptCli(sandbox);
      const absolutePath = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.res",
      );
      const resultFromRoot = await cliFromRoot.compilerArgs(absolutePath);

      // CLI from package dir with relative path
      const pkgDir = join(sandbox, "packages", "library");
      const cliFromPkg = createRescriptCli(pkgDir);
      const relativePath = join("src", "Library.res");
      const resultFromPkg = await cliFromPkg.compilerArgs(relativePath);

      expect(resultFromRoot.status).toBe(0);
      expect(resultFromPkg.status).toBe(0);

      // The stdout should be identical regardless of where we run from
      expect(resultFromRoot.stdout).toBe(resultFromPkg.stdout);
    }));

  it("does not load package sources when serving compiler args for an unloaded package", () =>
    runDaemonTest(async ({ sandbox, build, grpcClient }) => {
      // Build only from the "library" package — this loads library's sources but NOT app's
      const libDir = join(sandbox, "packages", "library");
      await build(libDir);

      // Ask for compiler args for a file in "app" — never loaded by the build
      const appFilePath = join(sandbox, "packages", "app", "src", "App.res");
      const response = await grpcClient.GetCompilerArgs({
        file_path: appFilePath,
      });

      // GetCompilerArgs reads config from disk, independent of loaded packages
      expect(response.compiler_args).toBeDefined();
      expect(response.compiler_args.length).toBeGreaterThan(0);
      expect(response.parser_args).toBeDefined();
      expect(response.parser_args.length).toBeGreaterThan(0);
    }));
});
