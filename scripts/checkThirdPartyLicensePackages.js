#!/usr/bin/env node

// @ts-check

import { execFileSync } from "node:child_process";

const notice = "THIRD_PARTY_LICENSES";
const platformPackages = [
  "@rescript/darwin-arm64",
  "@rescript/darwin-x64",
  "@rescript/linux-arm64",
  "@rescript/linux-x64",
  "@rescript/win32-x64",
];
const packagesWithoutNativeBinaries = [
  "rescript",
  "@rescript/runtime",
  "@rescript/belt",
];

/**
 * @param {string} pkg
 * @returns {Set<string>}
 */
function packedFiles(pkg) {
  const output = execFileSync(
    "yarn",
    ["workspace", pkg, "pack", "--json", "--dry-run"],
    { encoding: "utf8" },
  );
  return new Set(
    output
      .trim()
      .split("\n")
      .map(line => JSON.parse(line))
      .filter(line => "location" in line)
      .map(line => line.location),
  );
}

for (const pkg of platformPackages) {
  if (!packedFiles(pkg).has(notice)) {
    throw new Error(`${pkg} does not include ${notice}`);
  }
}

for (const pkg of packagesWithoutNativeBinaries) {
  if (packedFiles(pkg).has(notice)) {
    throw new Error(`${pkg} must not include native-binary notices`);
  }
}

console.log(
  "Third-party license notices are present in the expected packages.",
);
