import { describe, expect, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests the daemon's lifecycle management: health checks (Ping), client
// introspection (GetClients), and automatic shutdown when all clients
// disconnect. The daemon has no grace period — it exits immediately once
// the last client is gone.
//
// Note: These tests verify user-visible behavior only. The Ping and GetClients
// RPCs are simple pass-through operations without internal spans worth asserting.

describe("lifecycle", () => {
  it("responds to Ping", () =>
    runDaemonTest(async ({ grpcClient }) => {
      const response = await grpcClient.Ping({});
      expect(response.ready).toBe(true);
    }));

  it("lists connected clients via GetClients", () =>
    runDaemonTest(async ({ grpcClient }) => {
      const response = await grpcClient.GetClients({});
      expect(response.clients).toBeDefined();
    }));

  it("exits when all clients disconnect", () =>
    runDaemonTest(async ({ debug, daemon }) => {
      // Close the debug stream (the only connected client)
      debug.close();

      // Daemon should exit since no clients remain
      const deadline = Date.now() + 3000;
      let alive = true;
      while (Date.now() < deadline && alive) {
        try {
          // signal 0 checks if process exists without sending a signal
          process.kill(daemon.process.pid, 0);
          await new Promise(r => setTimeout(r, 100));
        } catch {
          alive = false;
        }
      }

      expect(alive).toBe(false);
    }));
});
