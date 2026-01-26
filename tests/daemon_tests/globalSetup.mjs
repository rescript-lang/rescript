/**
 * Vitest global setup - runs once before all tests.
 *
 * Cleans up any orphaned daemon processes that may have been left behind
 * from previous test runs that crashed or were interrupted.
 */

import { execSync } from "node:child_process";

export default async function globalSetup() {
  // Kill any orphaned rescript daemon processes
  // These might be left over from crashed tests or interrupted runs
  try {
    // Find and kill any rescript.exe daemon processes
    // The pattern matches the daemon command specifically
    if (process.platform === "darwin" || process.platform === "linux") {
      execSync("pkill -9 -f 'rescript.exe daemon' 2>/dev/null || true", {
        stdio: "ignore",
      });
    }
  } catch {
    // pkill returns non-zero if no processes matched, which is fine
  }

  // Small delay to ensure processes are fully cleaned up
  await new Promise(resolve => setTimeout(resolve, 100));
}
