import { describe, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { runDaemonTest } from "../helpers/test-context-v2.mjs";
import { waitForProcessExit } from "../helpers/wait.mjs";

// Tests watch mode: a watch client spawns via the CLI binary, connects to the
// daemon, and triggers an initial build. Verifies that the watch client emits
// the expected lifecycle events and that it disconnects cleanly when receiving
// SIGINT or SIGTERM (the standard ways to stop a watch process).

describe("watch", () => {
  it("watch client connects and performs initial build", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      const watchHandle = await watch(sandbox);
      watchHandle.stop();
    }));

  it("watch client disconnects on SIGINT", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);
      const watchProc = cli.spawnWatch();

      // Wait for the watch to be ready (initial build)
      await new Promise(r => setTimeout(r, 2000));

      watchProc.process.kill("SIGINT");
      await waitForProcessExit(watchProc.process);
    }));

  it("watch client disconnects on SIGTERM", () =>
    runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);
      const watchProc = cli.spawnWatch();

      // Wait for the watch to be ready (initial build)
      await new Promise(r => setTimeout(r, 2000));

      watchProc.process.kill("SIGTERM");
      await waitForProcessExit(watchProc.process);
    }));
});
