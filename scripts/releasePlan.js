#!/usr/bin/env node

import fs from "node:fs/promises";
import process from "node:process";
import { parseArgs } from "node:util";
import semver from "semver";

export function createReleasePlan(version, currentStable = false) {
  const parsedVersion = semver.parse(version);
  if (
    parsedVersion === null ||
    parsedVersion.version !== version ||
    parsedVersion.build.length > 0
  ) {
    throw new Error(`Invalid canonical version: ${version}`);
  }
  if (typeof currentStable !== "boolean") {
    throw new Error("currentStable must be a boolean.");
  }

  const isPrerelease = parsedVersion.prerelease.length > 0;
  if (currentStable && isPrerelease) {
    throw new Error("A prerelease cannot be the current stable release.");
  }

  const npmTag = isPrerelease
    ? `next-${parsedVersion.major}`
    : currentStable
      ? "latest"
      : `latest-${parsedVersion.major}`;

  return {
    version,
    gitTag: `v${version}`,
    currentStable,
    npmTag,
    publishTag: currentStable ? null : npmTag,
  };
}

export async function validateReleaseFiles(plan) {
  const packageJson = JSON.parse(await fs.readFile("package.json", "utf8"));
  if (packageJson.version !== plan.version) {
    throw new Error(
      `package.json has version ${packageJson.version}, expected ${plan.version}.`,
    );
  }

  const changelog = await fs.readFile("CHANGELOG.md", "utf8");
  const firstHeading = changelog.match(/^# (.+)$/m)?.[1];
  if (firstHeading !== plan.version) {
    throw new Error(
      `The first changelog version is ${firstHeading ?? "missing"}, expected ${plan.version}.`,
    );
  }
}

async function main() {
  const { values } = parseArgs({
    args: process.argv.slice(2),
    strict: true,
    options: {
      version: { type: "string", short: "v" },
      "current-stable": { type: "boolean" },
      validate: { type: "boolean" },
      "github-output": { type: "string" },
      "github-summary": { type: "string" },
    },
  });
  const version =
    values.version ??
    JSON.parse(await fs.readFile("package.json", "utf8")).version;
  const plan = createReleasePlan(version, Boolean(values["current-stable"]));
  if (values.validate) {
    await validateReleaseFiles(plan);
  }

  if (values["github-output"]) {
    const output = [
      `version=${plan.version}`,
      `git-tag=${plan.gitTag}`,
      `npm-tag=${plan.npmTag}`,
      `publish-tag=${plan.publishTag ?? ""}`,
    ].join("\n");
    await fs.appendFile(values["github-output"], `${output}\n`);
  }

  if (values["github-summary"]) {
    const summary = [
      "## Release plan",
      "",
      "| | |",
      "|-|-|",
      `| Version | \`${plan.version}\` |`,
      `| Git tag | \`${plan.gitTag}\` |`,
      `| npm tag | \`${plan.npmTag}\` |`,
      `| Source commit | \`${process.env.GITHUB_SHA ?? "local"}\` |`,
      "",
      "The protected `npm-release` job publishes this plan after approval.",
      "",
    ].join("\n");
    await fs.appendFile(values["github-summary"], summary);
  }

  console.log(JSON.stringify(plan, null, 2));
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error(error.message || error);
    process.exitCode = 1;
  });
}
