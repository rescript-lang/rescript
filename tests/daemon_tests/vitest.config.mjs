import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    testTimeout: 30_000,
    hookTimeout: 15_000,
    include: ["**/*.test.mjs"],
    globalSetup: "./globalSetup.mjs",
  },
});
