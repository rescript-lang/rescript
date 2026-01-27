import { describe, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests concurrent client access to the daemon. The core value proposition of
// the daemon architecture is eliminating lockfile conflicts: multiple CLI
// commands can run simultaneously against the same project without errors.
// These tests verify that concurrent builds, clean-then-build, and
// format-during-watch all work correctly.

describe("concurrent-clients", () => {
  it("two sequential builds both succeed", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      await cli.build();
      await cli.build();
    }));

  it("build succeeds while watch is running", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      const watchHandle = await watch(sandbox);

      try {
        const cli = createRescriptCli(sandbox);
        await cli.build();
      } finally {
        watchHandle.stop();
      }
    }));

  it("clean succeeds while watch is running", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      const watchHandle = await watch(sandbox);

      try {
        const cli = createRescriptCli(sandbox);
        await cli.clean();
      } finally {
        watchHandle.stop();
      }
    }));

  it("format succeeds while watch is running", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      const watchHandle = await watch(sandbox);

      try {
        const cli = createRescriptCli(sandbox);
        await cli.format();
      } finally {
        watchHandle.stop();
      }
    }));

  it("build after clean succeeds", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);

      // Build, clean, build again
      await cli.build();
      await cli.clean();
      await cli.build();
    }));
});
