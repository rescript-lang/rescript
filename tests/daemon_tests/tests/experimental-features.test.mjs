import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { startDaemon, stopDaemon } from "../helpers/daemon.mjs";
import { createOtelReceiver } from "../helpers/otel-receiver.mjs";
import { createRescriptCli } from "../helpers/process.mjs";
import { createSandbox, removeSandbox } from "../helpers/sandbox.mjs";
import { collectProcessOutput } from "../helpers/wait.mjs";

// Tests for experimental-features configuration handling.
// Experimental features are configured at the project root level.
// We test both standalone mode (no daemon) and daemon mode.

describe("experimental-features", () => {
  let sandbox;

  beforeEach(async () => {
    sandbox = await createSandbox();
  });

  afterEach(async () => {
    await removeSandbox(sandbox);
  });

  it("build succeeds with experimental syntax when feature is enabled (standalone)", async () => {
    const cli = createRescriptCli(sandbox);

    // Enable experimental features in root config
    const configPath = join(sandbox, "rescript.json");
    const config = JSON.parse(await readFile(configPath, "utf8"));
    config["experimental-features"] = { LetUnwrap: true };
    await writeFile(configPath, JSON.stringify(config, null, 2));

    // Add a file that uses experimental let? syntax
    const expFilePath = join(sandbox, "src", "Experimental.res");
    await writeFile(
      expFilePath,
      `type user = {
  address?: {
    city?: string,
  },
}

let userCity = (user: user): option<string> => {
  let? Some(address) = user.address
  let? Some(city) = address.city
  Some(city)
}
`,
    );

    const result = await cli.build();

    expect(result.status).toBe(0);
  });

  it("compiler-args includes experimental flags (standalone)", async () => {
    const cli = createRescriptCli(sandbox);

    // Enable experimental features in root config
    const configPath = join(sandbox, "rescript.json");
    const config = JSON.parse(await readFile(configPath, "utf8"));
    config["experimental-features"] = { LetUnwrap: true };
    await writeFile(configPath, JSON.stringify(config, null, 2));

    const filePath = join(sandbox, "src", "Root.res");
    const result = await cli.compilerArgs(filePath);

    expect(result.status).toBe(0);
    expect(result.stdout).toContain("-enable-experimental");
    expect(result.stdout).toContain("LetUnwrap");
  });

  it("compiler-args includes experimental flags (daemon mode)", async () => {
    const cli = createRescriptCli(sandbox);

    // Enable experimental features BEFORE starting daemon
    const configPath = join(sandbox, "rescript.json");
    const config = JSON.parse(await readFile(configPath, "utf8"));
    config["experimental-features"] = { LetUnwrap: true };
    await writeFile(configPath, JSON.stringify(config, null, 2));

    // Start otel receiver and daemon with otel enabled
    const otelReceiver = await createOtelReceiver();
    const daemon = await startDaemon(sandbox, {
      otelEndpoint: otelReceiver.endpoint,
    });

    try {
      const filePath = join(sandbox, "src", "Root.res");
      const result = await cli.compilerArgs(filePath);

      expect(result.status).toBe(0);
      expect(result.stdout).toContain("-enable-experimental");
      expect(result.stdout).toContain("LetUnwrap");

      // Verify the daemon processed the compiler-args request (internal behavior)
      await otelReceiver.waitForSpan(s => s.name === "rpc.get_compiler_args");
      const compilerArgsSpans = otelReceiver.getSpansByName(
        "rpc.get_compiler_args",
      );
      expect(compilerArgsSpans.length).toBe(1);
      // The span should have the file path attribute
      expect(compilerArgsSpans[0].attributes.file_path).toContain("Root.res");
    } finally {
      await stopDaemon(daemon);
      await otelReceiver.stop();
    }
  });

  it("build fails with invalid experimental-features format", async () => {
    const cli = createRescriptCli(sandbox);

    const configPath = join(sandbox, "rescript.json");
    const config = JSON.parse(await readFile(configPath, "utf8"));
    // Invalid: should be object, not array
    config["experimental-features"] = ["LetUnwrap"];
    await writeFile(configPath, JSON.stringify(config, null, 2));

    const result = await cli.build();

    expect(result.status).not.toBe(0);
    const output = result.stdout + result.stderr;
    expect(output).toContain("rescript.json");
  });

  it("compiler-args fails with unknown experimental feature key", async () => {
    const cli = createRescriptCli(sandbox);

    const configPath = join(sandbox, "rescript.json");
    const config = JSON.parse(await readFile(configPath, "utf8"));
    // Unknown feature key
    config["experimental-features"] = { FooBar: true };
    await writeFile(configPath, JSON.stringify(config, null, 2));

    const filePath = join(sandbox, "src", "Root.res");
    const result = await cli.compilerArgs(filePath);

    expect(result.status).not.toBe(0);
    const output = result.stdout + result.stderr;
    expect(output).toContain("Unknown experimental feature");
    expect(output).toContain("FooBar");
    expect(output).toContain("LetUnwrap");
  });

  it("build fails with unknown experimental feature key", async () => {
    const cli = createRescriptCli(sandbox);

    const configPath = join(sandbox, "rescript.json");
    const config = JSON.parse(await readFile(configPath, "utf8"));
    // Unknown feature key
    config["experimental-features"] = { InvalidFeature: true };
    await writeFile(configPath, JSON.stringify(config, null, 2));

    const result = await cli.build();

    expect(result.status).not.toBe(0);
    const output = result.stdout + result.stderr;
    expect(output).toContain("Unknown experimental feature");
    expect(output).toContain("InvalidFeature");
  });

  it("watch fails gracefully with invalid experimental-features format (no panic)", async () => {
    const cli = createRescriptCli(sandbox);

    const configPath = join(sandbox, "rescript.json");
    const config = JSON.parse(await readFile(configPath, "utf8"));
    // Invalid: should be object, not array
    config["experimental-features"] = ["LetUnwrap"];
    await writeFile(configPath, JSON.stringify(config, null, 2));

    // Spawn watch - it should fail quickly due to invalid config
    const watch = cli.spawnWatch();
    const result = await collectProcessOutput(watch);

    expect(result.status).not.toBe(0);
    const output = result.stdout + result.stderr;
    // Should mention the config file
    expect(output).toContain("rescript.json");
    // Should mention the parse error for experimental-features
    expect(output.toLowerCase()).toMatch(
      /experimental-features.*invalid|invalid.*experimental-features/i,
    );
    // Should NOT panic
    expect(output).not.toContain("panicked");
  });
});
