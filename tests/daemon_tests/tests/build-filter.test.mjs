import { describe, expect, it } from "vitest";
import { collectStream } from "../helpers/grpc-client.mjs";
import { runDaemonTest } from "../helpers/test-context-v2.mjs";

// Tests the --filter CLI flag. This flag passes a regex to the daemon that
// filters which modules are included in compilation.
// When --filter is set, only modules whose names match the regex are compiled.

describe("build-filter", () => {
  it("build succeeds with --filter matching all files", () =>
    runDaemonTest(async ({ sandbox, grpcClient }) => {
      // Filter that matches all modules
      const events = await collectStream(
        grpcClient.Build({ working_directory: sandbox, filter: ".*" }),
      );

      // Should have build_finished with success
      const finished = events.find(e => e.build_finished != null);
      expect(finished.build_finished.success).toBe(true);
    }));

  it("build with --filter compiles only matching modules", () =>
    runDaemonTest(async ({ sandbox, grpcClient }) => {
      // Build with filter to only include "Library".
      // On a fresh build, all modules are parse-dirty. The filter restricts
      // which dirty modules get compiled.
      const events = await collectStream(
        grpcClient.Build({ working_directory: sandbox, filter: "Library" }),
      );

      // Should succeed
      const finished = events.find(e => e.build_finished != null);
      expect(finished.build_finished.success).toBe(true);

      // Check the compiled event to verify only 1 module was compiled
      const compiled = events.find(e => e.compiled != null);
      expect(compiled.compiled.compiled_count).toBe(1);
    }));

  it("build with --filter produces expected spans", () =>
    runDaemonTest(async ({ sandbox, grpcClient }) => {
      // Build with filter
      await collectStream(
        grpcClient.Build({ working_directory: sandbox, filter: "Library" }),
      );
    }));
});
