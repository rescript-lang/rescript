import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    testTimeout: 45_000, // 45 second per-test timeout
    hookTimeout: 15_000, // 15 second hook timeout
    include: ["tests/**/*.test.mjs"],
    globalSetup: "./globalSetup.mjs",
  },
});
