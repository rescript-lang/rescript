#!/usr/bin/env node
/**
 * Tag a published version of the main ReScript packages with a given dist-tag.
 *
 * Usage:
 *   node scripts/npmRelease.js --version 12.0.1 --tag next
 *   node scripts/npmRelease.js --version 12.0.1 --tag latest --otp 123456
 *
 * - Runs `npm dist-tag add` for: rescript, @rescript/runtime, and all platform
 *   optional packages, reusing the same OTP so you only get prompted once.
 * - Pass `--dry-run` to see the commands without executing them.
 */
import { spawn } from "node:child_process";
import process from "node:process";
import readline from "node:readline/promises";
import { parseArgs } from "node:util";

const packages = [
  "rescript",
  "@rescript/runtime",
  "@rescript/darwin-arm64",
  "@rescript/darwin-x64",
  "@rescript/linux-arm64",
  "@rescript/linux-x64",
  "@rescript/win32-x64",
];

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

async function runDistTag(pkg, version, tag, otp, dryRun) {
  const spec = `${pkg}@${version}`;
  const args = ["dist-tag", "add", spec, tag, "--otp", otp];
  if (dryRun) {
    console.log(`[dry-run] npm ${args.join(" ")}`);
    return;
  }
  console.log(`Tagging ${spec} as ${tag}...`);
  await new Promise((resolve, reject) => {
    const child = spawn("npm", args, { stdio: "inherit" });
    child.on("exit", code => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`npm dist-tag failed for ${spec} (exit ${code})`));
      }
    });
    child.on("error", reject);
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
    const otp = await promptForOtp(values.otp);
    if (!otp) {
      throw new Error("OTP is required to publish dist-tags.");
    }
    for (const pkg of packages) {
      await runDistTag(
        pkg,
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
