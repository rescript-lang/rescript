import { stripVTControlCharacters } from "node:util";
import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("experimental features", () => {
  it("reports error for unknown experimental feature key", () =>
    runRewatchTest(async ({ cli, readFileInSandbox, writeFileInSandbox }) => {
      const config = JSON.parse(await readFileInSandbox("rescript.json"));
      config["experimental-features"] = { FooBar: true };
      await writeFileInSandbox(
        "rescript.json",
        JSON.stringify(config, null, 2) + "\n",
      );

      const result = await cli.build();
      expect(result.status).toBe(1);
      expect(result.stderr).toContain("Unknown experimental feature 'FooBar'");
    }));

  it("reports parse error for invalid experimental features format", () =>
    runRewatchTest(async ({ cli, readFileInSandbox, writeFileInSandbox }) => {
      const config = JSON.parse(await readFileInSandbox("rescript.json"));
      config["experimental-features"] = ["LetUnwrap"];
      await writeFileInSandbox(
        "rescript.json",
        JSON.stringify(config, null, 2) + "\n",
      );

      const result = await cli.build();
      expect(result.status).toBe(1);
      expect(result.stderr).toContain("Could not read rescript.json");
    }));
});

describe("after-build hook", () => {
  it("runs after-build command on successful build", () =>
    runRewatchTest(async ({ cli }) => {
      const result = await cli.build([
        "--after-build",
        "node -e console.log(0xCAFE)",
      ]);

      expect(result.status).toBe(0);
      expect(result.stdout).toContain("51966");
    }));

  it("does not run after-build command when build fails", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      await writeFileInSandbox(
        "packages/library/src/Library.res",
        "let x = DoesNotExist.value\n",
      );

      const result = await cli.build([
        "--after-build",
        "node -e console.log(0xCAFE)",
      ]);

      expect(result.status).toBe(1);
      expect(result.stdout).not.toContain("51966");
    }));
});

describe("warning configuration", () => {
  it("overrides warning config with --warn-error flag", () =>
    runRewatchTest(async ({ cli, readFileInSandbox, writeFileInSandbox }) => {
      const config = JSON.parse(await readFileInSandbox("rescript.json"));
      config.warnings = { number: "+8+27", error: "+27" };
      await writeFileInSandbox(
        "rescript.json",
        JSON.stringify(config, null, 2) + "\n",
      );

      const result = await cli.build(["--warn-error", "+3+8"]);

      expect(result.status).toBe(0);
    }));

  it("warn-error flag causes recompilation and is not sticky across builds", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      // Write a file that triggers warning 110 (%todo)
      await writeFileInSandbox("src/Root.res", "let todo = _ => %todo\n");

      // Build 1: normal build — should succeed with warning 110
      const b1 = await cli.build();
      expect(b1.status).toBe(0);
      expect(stripVTControlCharacters(b1.stderr)).toContain(
        "Warning number 110",
      );

      // Build 2: --warn-error +110 — should recompile and fail
      const b2 = await cli.build(["--warn-error", "+110"]);
      expect(b2.status).toBe(1);
      expect(stripVTControlCharacters(b2.stderr)).toContain(
        "Warning number 110 (configured as error)",
      );

      // Build 3: normal build again — should succeed (warn-error not sticky)
      const b3 = await cli.build();
      expect(b3.status).toBe(0);
    }));
});

describe("deprecated and unknown config fields", () => {
  it("warns about unsupported config fields", () =>
    runRewatchTest(async ({ cli, readFileInSandbox, writeFileInSandbox }) => {
      const config = JSON.parse(await readFileInSandbox("rescript.json"));
      config["ignored-dirs"] = ["node_modules"];
      await writeFileInSandbox(
        "rescript.json",
        JSON.stringify(config, null, 2) + "\n",
      );

      const result = await cli.build();
      expect(result.stderr).toContain("ignored-dirs");
      expect(result.stderr).toContain("is not supported");
    }));

  it("warns about unknown config fields", () =>
    runRewatchTest(async ({ cli, readFileInSandbox, writeFileInSandbox }) => {
      const config = JSON.parse(await readFileInSandbox("rescript.json"));
      config["some-unknown-field"] = "value";
      await writeFileInSandbox(
        "rescript.json",
        JSON.stringify(config, null, 2) + "\n",
      );

      const result = await cli.build();
      expect(result.stderr).toContain("some-unknown-field");
      expect(result.stderr).toContain("will be ignored");
    }));
});
