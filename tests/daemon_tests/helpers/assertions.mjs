import { globSync, readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const fixtureDir = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../fixture",
);

/**
 * Read all package names from the fixture's rescript.json dependencies + root.
 * @returns {string[]}
 */
export function getAllPackages() {
  const rootConfig = JSON.parse(
    readFileSync(join(fixtureDir, "rescript.json"), "utf-8"),
  );
  return [rootConfig.name, ...rootConfig.dependencies].sort();
}

/**
 * Count source files (.res) in the fixture.
 * This counts files that produce output (excludes namespace modules which are virtual).
 * @returns {number}
 */
export function countSourceFiles() {
  const notInLib = p => !p.includes("/lib/") && !p.startsWith("lib/");
  const resFiles = globSync("**/src/**/*.res", { cwd: fixtureDir }).filter(
    notInLib,
  );
  const testFiles = globSync("**/test/**/*.res", { cwd: fixtureDir }).filter(
    notInLib,
  );
  return resFiles.length + testFiles.length;
}

/**
 * Count total modules in the fixture by scanning source files and namespaces.
 * Each .res file is a module, and each package with a namespace adds one more.
 * @returns {number}
 */
export function countModules() {
  let count = countSourceFiles();

  // Add one module per namespaced package
  const packagesDir = join(fixtureDir, "packages");
  for (const pkg of readdirSync(packagesDir)) {
    const configPath = join(packagesDir, pkg, "rescript.json");
    const config = JSON.parse(readFileSync(configPath, "utf-8"));
    if (config.namespace) {
      count += 1;
    }
  }

  return count;
}
