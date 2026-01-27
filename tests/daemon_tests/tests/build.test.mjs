import { join } from "node:path";
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests the Build RPC: a client connects, the daemon compiles all packages in
// scope, and emits build lifecycle events. Verifies the basic happy path, that
// module counts are reported, and that a second build reuses the existing
// daemon state rather than re-initializing.

describe("build", () => {
  it("builds all packages from root", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      await build(sandbox);
    }));

  it("second build reuses daemon state", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      await build(sandbox);
      await build(sandbox);
    }));

  it("handles 4 concurrent builds without crashing", () =>
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
        processSpans: summary => {
          // Concurrent builds may complete in any order, so we just verify the count
          const buildCount = summary.filter(s =>
            s.includes("rpc.build"),
          ).length;
          return [`rpc.build x${buildCount}`];
        },
      },
    ));

  it("scoped build from child package only builds that package and deps", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      const appDir = join(sandbox, "packages", "app");
      await build(appDir);
    }));

  it("concurrent builds from different directories both succeed", () =>
    runDaemonTest(
      async ({ sandbox, build }) => {
        const appDir = join(sandbox, "packages", "app");
        const libraryDir = join(sandbox, "packages", "library");
        await Promise.all([build(appDir), build(libraryDir)]);
      },
      {
        processSpans: summary => {
          // Concurrent builds may complete in any order, so we just verify the count
          const buildCount = summary.filter(s =>
            s.includes("rpc.build"),
          ).length;
          return [`rpc.build x${buildCount}`];
        },
      },
    ));
});
