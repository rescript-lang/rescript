import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { createRescriptCli } from "../helpers/process.mjs";
import { createSandbox, removeSandbox } from "../helpers/sandbox.mjs";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests for standalone format mode (no daemon running).
// These verify that formatting works identically whether or not a daemon is present.
// Standalone tests use createSandbox() + createRescriptCli() directly (no runDaemonTest),
// so no daemon is started. Equivalence tests compare standalone vs daemon output.

describe("format-standalone", () => {
  let sandbox;

  beforeEach(async () => {
    sandbox = await createSandbox();
  });

  afterEach(async () => {
    await removeSandbox(sandbox);
  });

  // --- Standalone-only tests (no daemon running) ---

  it("formats project files from root (standalone)", async () => {
    const cli = createRescriptCli(sandbox);
    const result = await cli.format();
    expect(result.status).toBe(0);
  });

  it("formats a specific file (standalone)", async () => {
    const cli = createRescriptCli(sandbox);
    const filePath = join(sandbox, "src", "Root.res");

    // Mess up formatting
    await writeFile(filePath, "let    main   =    App.run()\n");

    const result = await cli.format([filePath]);
    expect(result.status).toBe(0);

    // Verify the file was reformatted
    const content = await readFile(filePath, "utf8");
    expect(content.trim()).toBe("let main = App.run()");
  });

  it("format --check exits 0 when files are formatted (standalone)", async () => {
    const cli = createRescriptCli(sandbox);
    const result = await cli.format(["--check"]);
    expect(result.status).toBe(0);
  });

  it("format --check exits non-zero when files need formatting (standalone)", async () => {
    const cli = createRescriptCli(sandbox);
    const filePath = join(sandbox, "src", "Root.res");

    // Mess up formatting
    await writeFile(filePath, "let    main   =    App.run()\n");

    const result = await cli.format(["--check", filePath]);
    expect(result.status).not.toBe(0);
  });

  it("formats stdin with --stdin .res (standalone)", async () => {
    const cli = createRescriptCli(sandbox);
    const result = await cli.formatStdin(".res", "let    x   =    1\n");
    expect(result.status).toBe(0);
    expect(result.stdout.trim()).toBe("let x = 1");
  });

  it("formats from child package scoped (standalone)", async () => {
    const libDir = join(sandbox, "packages", "library");
    const cli = createRescriptCli(libDir);
    const result = await cli.format();
    expect(result.status).toBe(0);
  });
});

describe("format-equivalence", () => {
  // --- Daemon vs standalone equivalence tests ---

  it("format stdin produces same output with and without daemon", async () => {
    const input = "let    x   =    1\n";

    // Standalone (no daemon)
    const standaloneSandbox = await createSandbox();
    let standaloneResult;
    try {
      const cli = createRescriptCli(standaloneSandbox);
      standaloneResult = await cli.formatStdin(".res", input);
    } finally {
      await removeSandbox(standaloneSandbox);
    }

    // With daemon (via runDaemonTest)
    let daemonResult;
    await runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);
      daemonResult = await cli.formatStdin(".res", input);
    });

    expect(standaloneResult.status).toBe(daemonResult.status);
    expect(standaloneResult.stdout).toBe(daemonResult.stdout);
  });

  it("format --check produces same exit code with and without daemon", async () => {
    // Standalone (no daemon) — files are already formatted
    const standaloneSandbox = await createSandbox();
    let standaloneResult;
    try {
      const cli = createRescriptCli(standaloneSandbox);
      standaloneResult = await cli.format(["--check"]);
    } finally {
      await removeSandbox(standaloneSandbox);
    }

    // With daemon
    let daemonResult;
    await runDaemonTest(async ({ sandbox }) => {
      const cli = createRescriptCli(sandbox);
      daemonResult = await cli.format(["--check"]);
    });

    expect(standaloneResult.status).toBe(daemonResult.status);
    expect(standaloneResult.status).toBe(0);
  });

  it("format --check reports same failures with and without daemon", async () => {
    // Standalone (no daemon) — mess up a file
    const standaloneSandbox = await createSandbox();
    let standaloneResult;
    try {
      const filePath = join(standaloneSandbox, "src", "Root.res");
      await writeFile(filePath, "let    main   =    App.run()\n");
      const cli = createRescriptCli(standaloneSandbox);
      standaloneResult = await cli.format(["--check"]);
    } finally {
      await removeSandbox(standaloneSandbox);
    }

    // With daemon — mess up the same file
    let daemonResult;
    await runDaemonTest(async ({ sandbox }) => {
      const filePath = join(sandbox, "src", "Root.res");
      await writeFile(filePath, "let    main   =    App.run()\n");
      const cli = createRescriptCli(sandbox);
      daemonResult = await cli.format(["--check"]);
    });

    expect(standaloneResult.status).toBe(daemonResult.status);
    expect(standaloneResult.status).not.toBe(0);

    // Both should report the file in stderr (normalize sandbox paths)
    const standaloneFiles = standaloneResult.stderr
      .split("\n")
      .filter(l => l.includes("[format check]"))
      .map(l => l.replace(/.*\[format check\]\s+/, ""))
      .map(l => l.replace(standaloneSandbox, "<sandbox>"));

    // For daemon result, extract the sandbox path from the runDaemonTest context
    const daemonFiles = daemonResult.stderr
      .split("\n")
      .filter(l => l.includes("[format check]"))
      .map(l => l.replace(/.*\[format check\]\s+/, ""))
      .map(l => l.replace(/\/.*?\//, "<sandbox>/"));

    // Both should report at least the Root.res file
    expect(standaloneFiles.length).toBeGreaterThan(0);
    expect(daemonFiles.length).toBeGreaterThan(0);

    // Normalize to just filenames for comparison
    const standaloneFileNames = standaloneFiles
      .map(f => f.split("/").pop())
      .sort();
    const daemonFileNames = daemonFiles.map(f => f.split("/").pop()).sort();
    expect(standaloneFileNames).toEqual(daemonFileNames);
  });

  it("format project collects same files with and without daemon", async () => {
    // Mess up ALL source files to see which ones get reported
    async function messUpFiles(sandboxDir) {
      const filesToMessUp = [
        join(sandboxDir, "src", "Root.res"),
        join(sandboxDir, "packages", "library", "src", "Library.res"),
      ];
      for (const f of filesToMessUp) {
        try {
          const content = await readFile(f, "utf8");
          // Add extra spaces to make it unformatted
          await writeFile(f, content.replace(/=/g, "   =   "));
        } catch {
          // File may not exist in some fixtures
        }
      }
    }

    // Standalone
    const standaloneSandbox = await createSandbox();
    let standaloneResult;
    try {
      await messUpFiles(standaloneSandbox);
      const cli = createRescriptCli(standaloneSandbox);
      standaloneResult = await cli.format(["--check"]);
    } finally {
      await removeSandbox(standaloneSandbox);
    }

    // With daemon
    let daemonResult;
    await runDaemonTest(async ({ sandbox }) => {
      await messUpFiles(sandbox);
      const cli = createRescriptCli(sandbox);
      daemonResult = await cli.format(["--check"]);
    });

    expect(standaloneResult.status).toBe(daemonResult.status);

    // Extract just filenames from [format check] lines
    const extractFileNames = stderr =>
      stderr
        .split("\n")
        .filter(l => l.includes("[format check]"))
        .map(l => l.replace(/.*\[format check\]\s+/, "").trim())
        .map(l => l.split("/").pop())
        .sort();

    const standaloneFileNames = extractFileNames(standaloneResult.stderr);
    const daemonFileNames = extractFileNames(daemonResult.stderr);
    expect(standaloneFileNames).toEqual(daemonFileNames);
  });
});
