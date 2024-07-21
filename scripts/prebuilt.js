#!/usr/bin/env node

// @ts-check

const fs = require("node:fs");
const assert = require("node:assert");
const semver = require("semver");
const { compilerVersionFile } = require("#dev/paths");
const { packageJson } = require("#dev/meta");

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

const pakageVersion = semver.parse(packageJson.version);
assert.ok(pakageVersion, "Failed to parse the version of the package.json");

assert.ok(
  verifyVersion(bsVersion, pakageVersion),
  `Bump the compiler version in ${compilerVersionFile}`,
);
