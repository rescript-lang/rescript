#!/usr/bin/env node

// @ts-check

import assert from "node:assert";
import fs from "node:fs";
import packageJson from "rescript/package.json" with { type: "json" };
import semver from "semver";
import { compilerVersionFile } from "#dev/paths";

/**
 * @param {semver.SemVer} bsVersion
 * @param {semver.SemVer} version
 */
function verifyVersion(bsVersion, version) {
  const { major, minor } = bsVersion;
  const { major: specifiedMajor, minor: specifiedMinor } = version;
  console.log(
    `Version check: package.json: ${specifiedMajor}.${specifiedMinor} vs ABI: ${major}.${minor}`,
  );
  return major === specifiedMajor && minor === specifiedMinor;
}

const bsVersionPattern = /let version = "(?<version>.*)"/m;
const bsVersionFileContent = fs.readFileSync(compilerVersionFile, "utf-8");
const bsVersionMatch = bsVersionFileContent.match(bsVersionPattern)?.groups;
assert.ok(bsVersionMatch, "Failed to parse the compiler version file");

const bsVersion = semver.parse(bsVersionMatch.version);
assert.ok(bsVersion, "Failed to parse the compiler version file");

const packageVersion = semver.parse(packageJson.version);
assert.ok(packageVersion, "Failed to parse the version of the package.json");

assert.ok(
  verifyVersion(bsVersion, packageVersion),
  `Bump the compiler version in ${compilerVersionFile}`,
);
