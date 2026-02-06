import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("watch", () => {
  it("watches for file changes and rebuilds", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      const watch = cli.spawnWatch();
      await watch.waitForOutput(/Finished initial compilation/i, 15000);

      await writeFileInSandbox(
        "packages/library/src/Library.res",
        'let greeting = "modified"\n',
      );

      await watch.waitForOutput(/Finished.*compilation/i, 10000);
    }));

  it("compiles newly created files", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox, fileExists }) => {
      const watch = cli.spawnWatch();
      await watch.waitForOutput(/Finished initial compilation/i, 15000);

      await writeFileInSandbox(
        "packages/library/src/NewFile.res",
        'let value = "hello from new file"\n',
      );

      await watch.waitForOutput(/Finished.*compilation/i, 10000);
      expect(fileExists("packages/library/src/NewFile.mjs")).toBe(true);
    }));

  it("warnings persist across incremental builds", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox }) => {
      const watch = cli.spawnWatch();
      await watch.waitForOutput(/Finished initial compilation/i, 15000);

      // Introduce a warning in Library.res (unused local variable)
      await writeFileInSandbox(
        "packages/library/src/Library.res",
        'let greeting = { let x = 42; "hello" }\n',
      );

      await watch.waitForOutput(/Finished.*compilation/i, 10000);
      let stderr = watch.getStderr();
      expect(stderr).toContain("unused variable x");

      // Modify a different file (App.res) — Library.res is not recompiled
      await writeFileInSandbox(
        "packages/app/src/App.res",
        'let run = () => Library.greeting ++ "!"\n',
      );

      await watch.waitForOutput(/Finished.*compilation/i, 10000);
      // The warning from Library.res should still be reported
      stderr = watch.getStderr();
      const matches = stderr.match(/unused variable x/g);
      expect(matches?.length).toBeGreaterThanOrEqual(2);
    }));

  it("triggers full rebuild on rescript.json change", () =>
    runRewatchTest(
      async ({ cli, readFileInSandbox, writeFileInSandbox, fileExists }) => {
        const watch = cli.spawnWatch();
        await watch.waitForOutput(/Finished initial compilation/i, 15000);

        expect(fileExists("src/Root.mjs")).toBe(true);

        // Change suffix in root rescript.json
        const config = JSON.parse(await readFileInSandbox("rescript.json"));
        config.suffix = ".res.mjs";
        await writeFileInSandbox(
          "rescript.json",
          JSON.stringify(config, null, 2) + "\n",
        );

        await watch.waitForOutput(/Finished.*compilation/i, 10000);
        // New suffix should be used for root package
        expect(fileExists("src/Root.res.mjs")).toBe(true);
      },
    ));

  it("ignores changes outside source dirs", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox, fileExists }) => {
      const watch = cli.spawnWatch();
      await watch.waitForOutput(/Finished initial compilation/i, 15000);

      // Create a .res file outside any source directory
      await writeFileInSandbox("random-dir/NotSource.res", "let x = 42\n");

      // Trigger a real source change to prove the watcher is alive
      await writeFileInSandbox(
        "src/WatchProbe.res",
        'let probe = "watcher-is-alive"\n',
      );

      await watch.waitForOutput(/Finished.*compilation/i, 10000);
      // Probe file should be compiled
      expect(fileExists("src/WatchProbe.mjs")).toBe(true);
      // File outside source dir should NOT be compiled
      expect(fileExists("random-dir/NotSource.mjs")).toBe(false);
    }));

  it("reports missing source folder without crashing", () =>
    runRewatchTest(
      async ({ cli, readFileInSandbox, writeFileInSandbox, fileExists }) => {
        // Add a non-existent source folder to the root config
        const config = JSON.parse(await readFileInSandbox("rescript.json"));
        config.sources = [{ dir: "nonexistent-folder" }, config.sources];
        await writeFileInSandbox(
          "rescript.json",
          JSON.stringify(config, null, 2) + "\n",
        );

        const watch = cli.spawnWatch();
        await watch.waitForOutput(/Finished initial compilation/i, 15000);

        // Watcher should still compile the valid sources despite the missing folder
        expect(fileExists("src/Root.mjs")).toBe(true);
      },
    ));

  it("recovers from invalid config change", () =>
    runRewatchTest(async ({ cli, readFileInSandbox, writeFileInSandbox }) => {
      const watch = cli.spawnWatch();
      await watch.waitForOutput(/Finished initial compilation/i, 15000);

      // Break the config with invalid experimental features
      const config = JSON.parse(await readFileInSandbox("rescript.json"));
      config["experimental-features"] = "not-a-valid-value";
      await writeFileInSandbox(
        "rescript.json",
        JSON.stringify(config, null, 2) + "\n",
      );

      // Watcher should report the error without crashing
      await watch.waitForOutput(/error/i, 10000);

      // Restore valid config
      delete config["experimental-features"];
      await writeFileInSandbox(
        "rescript.json",
        JSON.stringify(config, null, 2) + "\n",
      );

      // Watcher should recover and compile successfully
      await watch.waitForOutput(/Finished.*compilation/i, 10000);
    }));

  it("handles file rename during watch", () =>
    runRewatchTest(
      async ({ cli, writeFileInSandbox, deleteFile, fileExists }) => {
        const watch = cli.spawnWatch();
        await watch.waitForOutput(/Finished initial compilation/i, 15000);

        expect(fileExists("packages/library/src/Library.mjs")).toBe(true);

        // Rename Library.res to LibRenamed.res (write new + delete old)
        await writeFileInSandbox(
          "packages/library/src/LibRenamed.res",
          'let greeting = "renamed"\n',
        );
        await deleteFile("packages/library/src/Library.res");

        await watch.waitForOutput(/Finished.*compilation/i, 10000);

        // Old artifacts should be removed, new file should be compiled
        expect(fileExists("packages/library/src/Library.mjs")).toBe(false);
        expect(fileExists("packages/library/src/LibRenamed.mjs")).toBe(true);
      },
    ));

  it("removes artifacts when a source file is deleted", () =>
    runRewatchTest(async ({ cli, deleteFile, fileExists }) => {
      const watch = cli.spawnWatch();
      await watch.waitForOutput(/Finished initial compilation/i, 15000);

      expect(fileExists("packages/library/src/Library.mjs")).toBe(true);

      await deleteFile("packages/library/src/Library.res");

      await watch.waitForOutput(/Finished.*compilation/i, 10000);
      expect(fileExists("packages/library/src/Library.mjs")).toBe(false);
    }));
});
