#!/usr/bin/env node

// @ts-check

import { spawn } from "node:child_process";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import * as readline from "node:readline/promises";
import { artifactListFile, projectDir } from "#dev/paths";

/**
 * @typedef {(
 *   | { "base": string }
 *   | { "location": string }
 *   | { "output": string }
 * )} YarnPackOutputLine
 */

/**
 * @param {string} pkg
 */
async function getArtifacts(pkg) {
  const args = ["workspace", pkg, "pack", "--json", "--dry-run"];
  const files = [];

  const command =
    process.platform === "win32" ? (process.env.ComSpec ?? "cmd.exe") : "yarn";
  const commandArgs =
    process.platform === "win32"
      ? ["/d", "/s", "/c", "yarn.cmd", ...args]
      : args;
  const child = spawn(command, commandArgs, {
    stdio: ["ignore", "pipe", "inherit"],
  });

  const exitCode = new Promise((resolve, reject) => {
    child.once("error", reject);
    child.once("close", code => resolve(code));
  });

  for await (const line of readline.createInterface({
    input: child.stdout.setEncoding("utf8"),
    crlfDelay: Number.POSITIVE_INFINITY,
  })) {
    /** @type {YarnPackOutputLine} */
    const json = JSON.parse(line);
    if ("location" in json) {
      // Workaround for false positive reports
      // See https://github.com/yarnpkg/berry/issues/6766
      if (json.location.startsWith("_build")) {
        continue;
      }

      files.push(json.location);
    }
  }

  const code = await exitCode;
  if (code !== 0) {
    throw new Error(`yarn ${args.join(" ")} exited with status ${code}`);
  }

  return files;
}

/**
 * Cross-platform release binaries are downloaded only in the packaging job.
 * Locally, temporary empty files let Yarn report the intended package paths;
 * CI instead requires every declared executable to exist so this convenience
 * cannot hide a missing build artifact.
 *
 * @param {string} pkg
 */
async function getPlatformArtifacts(pkg) {
  const packageDir = path.join(projectDir, "packages", pkg);
  /** @type {{ publishConfig: { executableFiles: string[] } }} */
  const packageJson = JSON.parse(
    await fs.readFile(path.join(packageDir, "package.json"), "utf8"),
  );
  const executables = packageJson.publishConfig.executableFiles.map(file =>
    file.replace(/^\.\//, ""),
  );
  const placeholders = [];

  try {
    for (const executable of executables) {
      const executablePath = path.join(packageDir, executable);
      try {
        await fs.access(executablePath);
      } catch {
        if (process.env.CI === "true") {
          throw new Error(`Missing platform artifact: ${executablePath}`);
        }
        await fs.mkdir(path.dirname(executablePath), { recursive: true });
        try {
          const placeholder = await fs.open(executablePath, "wx");
          await placeholder.close();
          placeholders.push(executablePath);
        } catch (error) {
          const code =
            typeof error === "object" && error !== null && "code" in error
              ? error.code
              : undefined;
          if (code !== "EEXIST") {
            throw error;
          }
        }
      }
    }
    return await getArtifacts(pkg);
  } finally {
    await Promise.all(placeholders.map(file => fs.unlink(file)));
  }
}

/** @type {Record<string, string[]>} */
const artifactsPerPackage = {};

for (const pkg of ["rescript", "@rescript/runtime", "@rescript/belt"]) {
  artifactsPerPackage[pkg] = await getArtifacts(pkg);
}

for (const pkg of [
  "@rescript/darwin-arm64",
  "@rescript/darwin-x64",
  "@rescript/linux-arm64",
  "@rescript/linux-x64",
  "@rescript/win32-x64",
]) {
  artifactsPerPackage[pkg] = await getPlatformArtifacts(pkg);
}

await fs.writeFile(
  artifactListFile,
  JSON.stringify(artifactsPerPackage, null, 2),
);
