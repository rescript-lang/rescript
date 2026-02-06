import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("format", () => {
  it("does not rewrite already formatted files", () =>
    runRewatchTest(async ({ cli }) => {
      await cli.format();
    }));

  // 4.1 Stdin Formatting

  it("formats .res code from stdin", () =>
    runRewatchTest(async ({ cli }) => {
      const result = await cli.formatStdin(".res", "let    x   =    1\n");

      expect(result.status).toBe(0);
      expect(result.stdout).toBe("let x = 1\n");
    }));

  it("formats .resi code from stdin", () =>
    runRewatchTest(async ({ cli }) => {
      const result = await cli.formatStdin(".resi", "let    x  :    int\n");

      expect(result.status).toBe(0);
      expect(result.stdout).toBe("let x: int\n");
    }));

  // 4.2 File Formatting

  it("formats a single file", () =>
    runRewatchTest(
      async ({ sandbox, cli, writeFileInSandbox, readFileInSandbox }) => {
        await writeFileInSandbox(
          "packages/library/src/Library.res",
          'let    greeting   =   "hello from library"\n',
        );

        const filePath = join(sandbox, "packages/library/src/Library.res");
        const result = await cli.format([filePath]);

        expect(result.status).toBe(0);

        const content = await readFileInSandbox(
          "packages/library/src/Library.res",
        );
        expect(content).toBe('let greeting = "hello from library"\n');
      },
    ));

  it("formats all project files", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox, readFileInSandbox }) => {
      await writeFileInSandbox(
        "packages/library/src/Library.res",
        'let    greeting   =   "hello from library"\n',
      );
      await writeFileInSandbox(
        "packages/app/src/App.res",
        "let   run  =  ()  =>  Library.greeting\n",
      );

      const result = await cli.format();

      expect(result.status).toBe(0);

      const libraryContent = await readFileInSandbox(
        "packages/library/src/Library.res",
      );
      expect(libraryContent).toBe('let greeting = "hello from library"\n');
      const appContent = await readFileInSandbox("packages/app/src/App.res");
      expect(appContent).toBe("let run = () => Library.greeting\n");
    }));

  it("formats only current package when run from subdirectory", () =>
    runRewatchTest(
      async ({ createCli, writeFileInSandbox, readFileInSandbox }) => {
        const cli = createCli("packages/library");

        await writeFileInSandbox(
          "packages/library/src/Library.res",
          'let    greeting   =   "hello from library"\n',
        );
        await writeFileInSandbox(
          "packages/app/src/App.res",
          "let   run  =  ()  =>  Library.greeting\n",
        );

        const result = await cli.format();

        expect(result.status).toBe(0);

        // Library file (in scope) should be formatted
        const libraryContent = await readFileInSandbox(
          "packages/library/src/Library.res",
        );
        expect(libraryContent).toBe('let greeting = "hello from library"\n');

        // App file (out of scope) should remain unformatted
        const appContent = await readFileInSandbox("packages/app/src/App.res");
        expect(appContent).toBe("let   run  =  ()  =>  Library.greeting\n");
      },
    ));

  // 4.3 Check Mode

  it("check mode succeeds for correctly formatted files", () =>
    runRewatchTest(async ({ cli }) => {
      const result = await cli.format(["--check"]);

      expect(result.status).toBe(0);
    }));

  it("check mode fails for unformatted files", () =>
    runRewatchTest(async ({ cli, writeFileInSandbox, readFileInSandbox }) => {
      await writeFileInSandbox(
        "packages/library/src/Library.res",
        'let    greeting   =   "hello from library"\n',
      );

      const result = await cli.format(["--check"]);

      expect(result.status).toBe(1);
      expect(result.stderr).toContain("Library.res");
      expect(result.stderr).toContain("needs formatting");

      // Verify the file was NOT modified (check mode is read-only)
      const content = await readFileInSandbox(
        "packages/library/src/Library.res",
      );
      expect(content).toBe('let    greeting   =   "hello from library"\n');
    }));
});
