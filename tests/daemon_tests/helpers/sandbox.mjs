import { execSync } from "node:child_process";
import { cp, mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import path, { join } from "node:path";
import { fileURLToPath } from "node:url";

const fixtureDir = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../fixture",
);

export async function createSandbox() {
  const dir = await mkdtemp(join(tmpdir(), "daemon-test-"));
  // Use realpath to resolve symlinks (e.g., /var -> /private/var on macOS)
  // so path.relative() works correctly when comparing with absolute paths
  const realDir = await realpath(dir);
  await cp(fixtureDir, realDir, { recursive: true });

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
