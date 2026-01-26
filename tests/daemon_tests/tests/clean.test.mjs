import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { createSandbox, removeSandbox } from "../helpers/sandbox.mjs";
import { runDaemonTest } from "../helpers/test-context-v2.mjs";

// Tests the Clean command. Clean removes build artifacts from disk (compiler
// assets, generated JS files) but the daemon remains running and can build
// again afterwards. Scoped clean from a child package only cleans that package
// and its external dependencies, not sibling local packages.

describe("clean", () => {
  it("cleans from root after a build", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      await build(sandbox);
      await clean(sandbox);
    }));

  it("build succeeds after clean", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      await build(sandbox);
      await clean(sandbox);
      await build(sandbox);
    }));

  it("scoped clean from child package", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      const libDir = join(sandbox, "packages", "library");
      await build(sandbox);
      await clean(libDir);
    }));

  it("clean without prior build succeeds", () =>
    runDaemonTest(async ({ sandbox, clean }) => {
      await clean(sandbox);
    }));

  it("scoped clean does not clean sibling local packages", () =>
    runDaemonTest(async ({ sandbox, build, clean, fileExists }) => {
      const libraryDir = join(sandbox, "packages", "library");

      await build(sandbox);

      // Verify artifacts exist before clean
      const appArtifact = join(sandbox, "packages", "app", "src", "App.mjs");
      const namespacedArtifact = join(
        sandbox,
        "packages",
        "namespaced",
        "src",
        "Helper.mjs",
      );
      expect(fileExists(appArtifact)).toBe(true);
      expect(fileExists(namespacedArtifact)).toBe(true);

      await clean(libraryDir);

      // Library's artifact should be cleaned
      const libraryArtifact = join(
        sandbox,
        "packages",
        "library",
        "src",
        "Library.mjs",
      );
      expect(fileExists(libraryArtifact)).toBe(false);

      // Sibling packages' artifacts should NOT be cleaned
      expect(fileExists(appArtifact)).toBe(true);
      expect(fileExists(namespacedArtifact)).toBe(true);
    }));

  it("handles concurrent builds without race conditions", () =>
    runDaemonTest(
      async ({ sandbox, build }) => {
        await Promise.all([
          build(sandbox),
          build(sandbox),
          build(sandbox),
          build(sandbox),
        ]);
      },
      {
        // Concurrent builds may complete in any order, so we just verify count
        processSpans: summary => {
          const buildCount = summary.filter(s =>
            s.includes("rpc.build"),
          ).length;
          return [`rpc.build x${buildCount}`];
        },
      },
    ));

  it("concurrent builds from different directories", () =>
    runDaemonTest(
      async ({ sandbox, build }) => {
        const appDir = join(sandbox, "packages", "app");
        const libraryDir = join(sandbox, "packages", "library");
        await Promise.all([build(appDir), build(libraryDir)]);
      },
      {
        // Concurrent builds may complete in any order
        processSpans: summary => {
          const buildCount = summary.filter(s =>
            s.includes("rpc.build"),
          ).length;
          return [`rpc.build x${buildCount}`];
        },
      },
    ));

  // This test checks CLI stdout/stderr output, which doesn't fit the span model.
  // We run it outside of runDaemonTest to verify the CLI behavior directly.
  it("rebuild after clean does not report compiler update", async () => {
    const sandbox = await createSandbox();
    try {
      const cli = createRescriptCli(sandbox);
      await cli.clean();
      const result = await cli.build();
      expect(result.status).toBe(0);
      const combined = result.stdout + result.stderr;
      expect(combined).not.toContain(
        "Cleaned previous build due to compiler update",
      );
    } finally {
      await removeSandbox(sandbox);
    }
  });
});
