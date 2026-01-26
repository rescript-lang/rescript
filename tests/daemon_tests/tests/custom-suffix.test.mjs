import { glob, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { countSourceFiles } from "../helpers/assertions.mjs";
import { runDaemonTest } from "../helpers/test-context-v2.mjs";

// Tests that the daemon detects rescript.json changes in watch mode and
// triggers a rebuild. The suffix setting is a root-level config option
// (per MonorepoSupport.md), so changing it in the root rescript.json should
// cause the watch client to send a ConfigChangeNotification, the daemon to
// re-read the config, and a full rebuild to produce files with the new suffix.

describe("custom-suffix", () => {
  const TOTAL_SOURCE_FILES = countSourceFiles();

  it("watch mode detects config change and rebuilds with new suffix", () =>
    runDaemonTest(async ({ sandbox, watch }) => {
      const watchHandle = await watch(sandbox);

      try {
        // The initial build produces .mjs files (default suffix)
        // Count matches source files (namespace modules don't produce output files)
        const mjsBefore = await findFiles(sandbox, "**/*.mjs", isSourceOutput);
        expect(mjsBefore.length).toBe(TOTAL_SOURCE_FILES);

        // Change suffix in the root rescript.json (suffix is a root-level setting)
        const rootConfigPath = join(sandbox, "rescript.json");
        const content = await readFile(rootConfigPath, "utf8");
        const config = JSON.parse(content);
        config.suffix = ".res.js";
        await writeFile(rootConfigPath, JSON.stringify(config, null, 2));

        // Wait for rebuild triggered by config change detection
        await watchHandle.waitForBuild();

        // Verify .res.js files were created (same count as source files)
        const resJsFiles = await findFiles(
          sandbox,
          "**/*.res.js",
          isSourceOutput,
        );
        expect(resJsFiles.length).toBe(TOTAL_SOURCE_FILES);
      } finally {
        watchHandle.stop();
      }
    }));
});

/**
 * Filter for in-source compiled output files.
 * Only matches files in src/ or test/ directories (the actual in-source output location).
 * Excludes node_modules and lib/bs directories.
 * @param {string} path
 * @returns {boolean}
 */
function isSourceOutput(path) {
  // Must be in a src/ or test/ directory (in-source output)
  // Handle both "src/..." (root) and ".../src/..." (packages)
  const inSourceDir =
    path.includes("/src/") ||
    path.includes("/test/") ||
    path.startsWith("src/") ||
    path.startsWith("test/");
  // Exclude node_modules and lib/bs
  const notExcluded =
    !path.includes("node_modules") &&
    !path.includes("/lib/") &&
    !path.startsWith("lib/");
  return inSourceDir && notExcluded;
}

/**
 * Find files matching a glob pattern under a directory.
 * @param {string} dir
 * @param {string} pattern
 * @param {(path: string) => boolean} [filter] - Optional filter function
 * @returns {Promise<string[]>}
 */
async function findFiles(dir, pattern, filter = () => true) {
  const results = [];
  for await (const entry of glob(pattern, { cwd: dir })) {
    if (filter(entry)) {
      results.push(entry);
    }
  }
  return results;
}
