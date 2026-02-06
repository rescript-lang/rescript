import { readdirSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("build", () => {
  it("builds all packages from root", () =>
    runRewatchTest(async ({ cli }) => {
      await cli.build();
    }));

  it("builds from a package subdirectory", () =>
    runRewatchTest(async ({ createCli, fileExists }) => {
      const libCli = createCli("packages/library");
      const result = await libCli.build();

      expect(result.status).toBe(0);
      expect(fileExists("packages/library/src/Library.mjs")).toBe(true);
    }));

  it("does not produce new artifacts on a second build", () =>
    runRewatchTest(async ({ cli, sandbox }) => {
      await cli.build();

      // Collect all .mjs files after first build
      const collectMjs = dir => {
        const results = [];
        for (const entry of readdirSync(dir, { withFileTypes: true })) {
          const full = join(dir, entry.name);
          if (
            entry.isDirectory() &&
            entry.name !== "node_modules" &&
            entry.name !== ".yarn"
          ) {
            results.push(...collectMjs(full));
          } else if (entry.name.endsWith(".mjs")) {
            results.push(full);
          }
        }
        return results;
      };

      const filesAfterFirst = collectMjs(sandbox).sort();

      // Build again
      await cli.build();

      const filesAfterSecond = collectMjs(sandbox).sort();

      // No new files should appear
      expect(filesAfterSecond).toEqual(filesAfterFirst);
    }));

  it("only compiles files matching the filter regex", () =>
    runRewatchTest(async ({ cli, fileExists }) => {
      const result = await cli.build(["--filter", "Library"]);

      expect(result.status).toBe(0);
      // Library matches the filter, so it should be compiled
      expect(fileExists("packages/library/src/Library.mjs")).toBe(true);
      // App and Root do not match, so they should not be compiled
      expect(fileExists("packages/app/src/App.mjs")).toBe(false);
      expect(fileExists("src/Root.mjs")).toBe(false);
    }));
});
