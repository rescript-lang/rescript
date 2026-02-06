import { execSync } from "node:child_process";
import { cp, mkdtemp, readdir, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path, { join } from "node:path";
import { fileURLToPath } from "node:url";

const fixtureDir = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../fixture",
);

/**
 * Check that the fixture directory is clean (no build artifacts).
 * This prevents local builds from interfering with tests.
 */
async function assertFixtureClean() {
  const artifacts = [];

  async function checkDir(dir) {
    const entries = await readdir(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = join(dir, entry.name);
      if (entry.isDirectory()) {
        if (entry.name === "lib") {
          artifacts.push(path.relative(fixtureDir, fullPath));
        } else if (entry.name !== "node_modules" && entry.name !== ".yarn") {
          await checkDir(fullPath);
        }
      } else if (entry.name.endsWith(".mjs") || entry.name.endsWith(".bs.js")) {
        artifacts.push(path.relative(fixtureDir, fullPath));
      }
    }
  }

  await checkDir(fixtureDir);

  if (artifacts.length > 0) {
    throw new Error(
      `Fixture directory contains build artifacts. Please clean before running tests:\n` +
        `  cd tests/rewatch_tests/fixture && rm -rf packages/*/lib src/*.mjs packages/*/src/*.mjs packages/*/src/*.bs.js\n\n` +
        `Found:\n  ${artifacts.join("\n  ")}`,
    );
  }
}

export async function createSandbox() {
  // Ensure fixture is clean before copying
  await assertFixtureClean();

  const dir = await mkdtemp(join(tmpdir(), "rewatch-test-"));
  // Use realpath to resolve symlinks (e.g., /var -> /private/var on macOS)
  // so path.relative() works correctly when comparing with absolute paths
  const realDir = await realpath(dir);
  // Copy the fixture directory to the sandbox, excluding node_modules
  // (yarn install --immutable below will recreate it from cache)
  await cp(fixtureDir, realDir, {
    recursive: true,
    filter: src => !src.includes("node_modules"),
  });

  // Run yarn install to create proper node_modules workspace symlinks.
  // rewatch resolves dependencies via node_modules/<pkgName> and checks if
  // the canonical path is inside the workspace root (no node_modules segment)
  // to determine if a package is "local". See MonorepoSupport.md.
  // Use the bundled yarn binary directly to avoid reliance on the system yarn
  // (which may be Yarn 1 classic if corepack is not enabled).
  try {
    execSync("node .yarn/releases/yarn-4.12.0.cjs install", {
      cwd: realDir,
      stdio: "pipe",
    });
  } catch (err) {
    const stderr = err.stderr?.toString() || "";
    const stdout = err.stdout?.toString() || "";
    throw new Error(
      `yarn install failed in sandbox ${realDir}:\n${stderr}\n${stdout}`,
    );
  }

  return realDir;
}

export async function removeSandbox(dir) {
  if (!dir) return;
  await rm(dir, { recursive: true, force: true });
}
