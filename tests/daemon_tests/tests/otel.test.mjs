/**
 * OpenTelemetry integration tests.
 *
 * These tests verify that the daemon emits tracing spans that can be
 * collected via OTLP. The snapshot captures the span tree structure.
 */

import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

describe("otel", () => {
  it("emits spans for build operations", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      await build(sandbox);
    }));

  it("emits spans for clean operations", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      await build(sandbox);
      await clean(sandbox);
    }));

  it("emits spans for watch operations", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      const watchHandle = await watch(sandbox);
      watchHandle.stop();
    }));
});
