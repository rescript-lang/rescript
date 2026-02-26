#!/usr/bin/env node

const fs = require("fs");
const path = require("path");

const version = process.argv[2];

if (!version) {
  console.error("Usage: node rename-packages.js <version>");
  process.exit(1);
}

const packagesDir = path.join(__dirname, "..", "packages", "@rescript");
const rootPackageJson = path.join(__dirname, "..", "package.json");

// Platform package directories
const platformPackages = [
  "darwin-arm64",
  "darwin-x64",
  "linux-arm64",
  "linux-x64",
  "win32-x64",
];

// Update root package.json
console.log("Updating root package.json...");
const rootPkg = JSON.parse(fs.readFileSync(rootPackageJson, "utf8"));
rootPkg.name = "@roshan84ya/rescript";
rootPkg.version = version;

// Update optionalDependencies to use @roshan84ya scope
if (rootPkg.optionalDependencies) {
  const newDeps = {};
  for (const [key, val] of Object.entries(rootPkg.optionalDependencies)) {
    // Replace @rescript/ with @roshan84ya/rescript- for platform packages
    if (key.startsWith("@rescript/")) {
      const platform = key.replace("@rescript/", "");
      newDeps[`@roshan84ya/rescript-${platform}`] = version;
    } else {
      newDeps[key] = val;
    }
  }
  rootPkg.optionalDependencies = newDeps;
}

// Remove provenance for local/custom publishing
if (rootPkg.publishConfig) {
  delete rootPkg.publishConfig.provenance;
}

fs.writeFileSync(rootPackageJson, JSON.stringify(rootPkg, null, 2) + "\n");
console.log("  -> Updated root package.json");

// Update each platform package
for (const platform of platformPackages) {
  const packageJsonPath = path.join(packagesDir, platform, "package.json");

  if (!fs.existsSync(packageJsonPath)) {
    console.warn(`  -> Warning: ${packageJsonPath} not found, skipping`);
    continue;
  }

  console.log(`Updating packages/@rescript/${platform}/package.json...`);
  const pkg = JSON.parse(fs.readFileSync(packageJsonPath, "utf8"));

  // Rename to @roshan84ya scope
  pkg.name = `@roshan84ya/rescript-${platform}`;
  pkg.version = version;

  // Remove provenance
  if (pkg.publishConfig) {
    delete pkg.publishConfig.provenance;
  }

  fs.writeFileSync(packageJsonPath, JSON.stringify(pkg, null, 2) + "\n");
  console.log(`  -> Updated ${platform}`);
}

// Update runtime package if needed
const runtimePackageJsonPath = path.join(
  __dirname,
  "..",
  "packages",
  "@rescript",
  "runtime",
  "package.json"
);
if (fs.existsSync(runtimePackageJsonPath)) {
  console.log("Updating runtime package.json...");
  const runtimePkg = JSON.parse(
    fs.readFileSync(runtimePackageJsonPath, "utf8")
  );
  runtimePkg.name = "@roshan84ya/rescript-runtime";
  runtimePkg.version = version;

  if (runtimePkg.publishConfig) {
    delete runtimePkg.publishConfig.provenance;
  }

  fs.writeFileSync(
    runtimePackageJsonPath,
    JSON.stringify(runtimePkg, null, 2) + "\n"
  );
  console.log("  -> Updated runtime");
}

console.log("\nAll packages updated to version", version);
console.log("Packages will be published under @roshan84ya scope");
