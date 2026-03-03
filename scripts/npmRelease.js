#!/usr/bin/env node
/**
 * Tag a published version of the main ReScript packages with a given dist-tag.
 *
 * Usage:
 *   node scripts/npmRelease.js --version 12.0.1 --tag next
 *   node scripts/npmRelease.js --version 12.0.1 --tag latest --otp 123456
 *   node scripts/npmRelease.js --version 12.0.1 --tag latest --token <token>
 *
 * - Runs `npm dist-tag add` for every non-private workspace (same as CI publish)
 *   reusing the same OTP (when provided) so you only get prompted once.
 * - If no OTP is provided, npm uses the current auth flow (for example passkey).
 * - Token auth can be passed with `--token` or via `NODE_AUTH_TOKEN`/`NPM_TOKEN`.
 * - Pass `--dry-run` to see the commands without executing them.
 */
import process from "node:process";
import { parseArgs } from "node:util";
import { npm, yarn } from "../lib_dev/process.js";

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

function resolveAuth(values) {
  const otp = values.otp?.trim() || undefined;
  const token =
    values.token?.trim() ||
    process.env.NODE_AUTH_TOKEN?.trim() ||
    process.env.NPM_TOKEN?.trim() ||
    undefined;

  if (otp && token) {
    throw new Error("Use either --otp or token auth, not both.");
  }

  return { otp, token };
}

async function runDistTag(pkgName, version, tag, otp, token, dryRun) {
  const spec = `${pkgName}@${version}`;
  const args = ["dist-tag", "add", spec, tag];
  if (otp) {
    args.push("--otp", otp);
  }
  if (dryRun) {
    console.log(`[dry-run] npm ${args.join(" ")}`);
    return;
  }
  console.log(`Tagging ${spec} as ${tag}...`);
  const env = token
    ? {
        ...process.env,
        NODE_AUTH_TOKEN: token,
      }
    : process.env;
  await npm("dist-tag", args.slice(1), {
    env,
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
        token: { type: "string" },
        "dry-run": { type: "boolean" },
      },
    });
    if (!values.version || !values.tag) {
      console.error(
        "Usage: node scripts/npmRelease.js --version <version> --tag <tag> [--otp <code> | --token <token>] [--dry-run]",
      );
      process.exitCode = 1;
      return;
    }
    const workspaces = await getPublicWorkspaces();
    if (workspaces.length === 0) {
      throw new Error("No public workspaces found.");
    }

    const { otp, token } = resolveAuth(values);
    for (const workspace of workspaces) {
      await runDistTag(
        workspace,
        values.version,
        values.tag,
        otp,
        token,
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
