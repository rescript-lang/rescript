#!/usr/bin/env node
/**
 * Tag a published version of the main ReScript packages with a given dist-tag.
 *
 * Usage:
 *   node scripts/npmRelease.js --version 12.0.1 --tag next
 *   node scripts/npmRelease.js --version 12.0.1 --tag latest --otp 123456
 *
 * - Runs `npm dist-tag add` for every non-private workspace (same as CI publish)
 *   reusing the same OTP so you only get prompted once.
 * - Pass `--dry-run` to see the commands without executing them.
 */
import process from "node:process";
import readline from "node:readline/promises";
import { parseArgs } from "node:util";
import { npm, yarn } from "../lib_dev/process.js";

async function promptForOtp(existingOtp) {
  if (existingOtp) {
    return existingOtp;
  }
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  const answer = await rl.question("npm one-time password: ");
  rl.close();
  return answer.trim();
}

async function getPublicWorkspaces() {
  const { stdout } = await yarn("workspaces", [
    "list",
    "--no-private",
    "--json",
  ]);
  return stdout
    .split("\n")
    .filter(Boolean)
    .map(line => JSON.parse(line))
    .map(entry => entry.name);
}

async function runDistTag(pkgName, version, tag, otp, dryRun) {
  const spec = `${pkgName}@${version}`;
  const args = ["dist-tag", "add", spec, tag, "--otp", otp];
  if (dryRun) {
    console.log(`[dry-run] npm ${args.join(" ")}`);
    return;
  }
  console.log(`Tagging ${spec} as ${tag}...`);
  await npm("dist-tag", ["add", spec, tag, "--otp", otp], {
    stdio: "inherit",
    throwOnFail: true,
  });
}

async function main() {
  try {
    const { values } = parseArgs({
      args: process.argv.slice(2),
      strict: true,
      options: {
        version: { type: "string", short: "v" },
        tag: { type: "string", short: "t" },
        otp: { type: "string" },
        "dry-run": { type: "boolean" },
      },
    });
    if (!values.version || !values.tag) {
      console.error(
        "Usage: node scripts/npmRelease.js --version <version> --tag <tag> [--otp <code>] [--dry-run]",
      );
      process.exitCode = 1;
      return;
    }
    const workspaces = await getPublicWorkspaces();
    if (workspaces.length === 0) {
      throw new Error("No public workspaces found.");
    }

    const otp = await promptForOtp(values.otp);
    if (!otp) {
      throw new Error("OTP is required to publish dist-tags.");
    }
    for (const workspace of workspaces) {
      await runDistTag(
        workspace,
        values.version,
        values.tag,
        otp,
        Boolean(values["dry-run"]),
      );
    }
    if (values["dry-run"]) {
      console.log("Dry run complete.");
    } else {
      console.log("All packages tagged successfully.");
    }
  } catch (error) {
    console.error(error.message || error);
    process.exitCode = 1;
  }
}

await main();
