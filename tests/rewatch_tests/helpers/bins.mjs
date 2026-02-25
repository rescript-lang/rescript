import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const projectRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../../..",
);

const fixtureDir = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../fixture",
);

// Map Node.js platform/arch to rescript package target names
function getNodeTarget() {
  const platform = process.platform;
  const arch = process.arch;

  if (platform === "darwin" && arch === "x64") return "darwin-x64";
  if (platform === "darwin" && arch === "arm64") return "darwin-arm64";
  if (platform === "linux" && arch === "x64") return "linux-x64";
  if (platform === "linux" && arch === "arm64") return "linux-arm64";
  if (platform === "win32" && arch === "x64") return "win32-x64";

  return `${platform}-${arch}`;
}

const nodeTarget = getNodeTarget();

// Check if rescript is installed in the fixture's node_modules (e.g. via pkg.pr.new in CI).
// When present, use the installed package binaries and runtime.
// Otherwise fall back to the local development build.
const installedBinDir = path.join(
  fixtureDir,
  "node_modules",
  "@rescript",
  nodeTarget,
  "bin",
);
const useInstalled = fs.existsSync(installedBinDir);

function resolve() {
  if (useInstalled) {
    return {
      rescript_exe:
        process.env.REWATCH_EXECUTABLE ||
        path.join(installedBinDir, "rescript.exe"),
      bsc_exe:
        process.env.RESCRIPT_BSC_EXE || path.join(installedBinDir, "bsc.exe"),
      runtimePath:
        process.env.RESCRIPT_RUNTIME ||
        path.join(fixtureDir, "node_modules", "@rescript", "runtime"),
    };
  }

  // Fall back to local development build
  const rewatchProfile = process.env.CI ? "release" : "debug";
  const binDir = path.join(
    projectRoot,
    "packages",
    "@rescript",
    nodeTarget,
    "bin",
  );

  return {
    rescript_exe:
      process.env.REWATCH_EXECUTABLE ||
      path.join(projectRoot, "rewatch", "target", rewatchProfile, "rescript"),
    bsc_exe: process.env.RESCRIPT_BSC_EXE || path.join(binDir, "bsc.exe"),
    runtimePath:
      process.env.RESCRIPT_RUNTIME ||
      path.join(projectRoot, "packages", "@rescript", "runtime"),
  };
}

export const { rescript_exe, bsc_exe, runtimePath } = resolve();

export function logResolvedPaths() {
  const cyan = "\x1b[36m";
  const green = "\x1b[32m";
  const dim = "\x1b[2m";
  const bold = "\x1b[1m";
  const reset = "\x1b[0m";

  console.log(`\n${cyan}${bold}ReScript Test Toolchain${reset}`);
  if (useInstalled) {
    const pkgJsonPath = path.join(
      fixtureDir,
      "node_modules",
      "rescript",
      "package.json",
    );
    const pkg = JSON.parse(fs.readFileSync(pkgJsonPath, "utf8"));
    // Extract commit SHA from pkg.pr.new dependency URLs if available
    const runtimeUrl = pkg.dependencies?.["@rescript/runtime"] || "";
    const shaMatch = runtimeUrl.match(/@([0-9a-f]{7,40})$/);
    const label = shaMatch
      ? `${pkg.version} @ ${shaMatch[1].slice(0, 7)}`
      : pkg.version;
    console.log(
      `  ${green}mode${reset}         installed package (${bold}${label}${reset})`,
    );
  } else {
    console.log(`  ${green}mode${reset}         local dev build`);
  }
  console.log(
    `  ${green}rescript_exe${reset} ${dim}${path.resolve(rescript_exe)}${reset}`,
  );
  console.log(
    `  ${green}bsc_exe${reset}      ${dim}${path.resolve(bsc_exe)}${reset}`,
  );
  console.log(
    `  ${green}runtime${reset}      ${dim}${path.resolve(runtimePath)}${reset}`,
  );
  console.log();
}
