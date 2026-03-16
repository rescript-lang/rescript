import { readdirSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("clean", () => {
  it("cleans build artifacts", () =>
    runRewatchTest(async ({ cli }) => {
      await cli.build();
      await cli.clean();
    }));

  it("cleans only the scoped package when run from a subdirectory", () =>
    runRewatchTest(async ({ cli, createCli, fileExists }) => {
      await cli.build();

      // Verify build artifacts exist for both packages
      expect(fileExists("packages/library/src/Library.mjs")).toBe(true);
      expect(fileExists("packages/app/src/App.mjs")).toBe(true);

      // Clean only the library package
      const libCli = createCli("packages/library");
      await libCli.clean();

      // Library artifacts should be removed
      expect(fileExists("packages/library/src/Library.mjs")).toBe(false);

      // App artifacts should still be present
      expect(fileExists("packages/app/src/App.mjs")).toBe(true);
    }));

  it("cleans dependency build artifacts from node_modules", () =>
    runRewatchTest(async ({ sandbox, createCli }) => {
      const depsCli = createCli("packages/with-deps");
      await depsCli.build();

      const ocamlDir = join(sandbox, "node_modules/rescript-bun/lib/ocaml");

      // Verify rescript-bun dependency has build artifacts
      const filesBefore = readdirSync(ocamlDir);
      expect(filesBefore.length).toBeGreaterThan(0);

      await depsCli.clean();

      // Dependency build artifacts should be removed
      const filesAfter = readdirSync(ocamlDir);
      expect(filesAfter.length).toBe(0);
    }));

  it("does not report compiler update after explicit clean and rebuild", () =>
    runRewatchTest(async ({ cli }) => {
      await cli.build();
      await cli.clean();

      const result = await cli.build();

      expect(result.status).toBe(0);
      expect(result.stdout).not.toContain(
        "Cleaned previous build due to compiler update",
      );
    }));
});
