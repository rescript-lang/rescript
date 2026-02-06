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
      // First build to ensure lib/bs exists
      await cli.build();

      // Get compiler args for a source file
      const filePath = path.join(sandbox, "packages/library/src/Library.res");
      const result = await cli.compilerArgs(filePath);
      const json = JSON.parse(result.stdout);
      expect(normalizeCompilerArgs(json, sandbox)).toMatchSnapshot();
    }));
});
