import { existsSync, readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import { describe, expect, it } from "vitest";
import { runLspTest } from "../../helpers/test-context.mjs";

/**
 * Collect all files under a directory recursively.
 * Returns paths relative to the given root.
 */
function collectFiles(dir, root = dir) {
  const results = [];
  if (!existsSync(dir)) return results;

  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      results.push(...collectFiles(fullPath, root));
    } else {
      results.push(path.relative(root, fullPath));
    }
  }
  return results.sort();
}

/**
 * For a given package path in the sandbox, collect the file extensions
 * present in lib/lsp/ (sorted, deduped).
 */
function getArtifactExtensions(sandboxPath, packageDir) {
  const lspDir = path.join(sandboxPath, packageDir, "lib", "lsp");
  const files = collectFiles(lspDir);
  return new Set(files.map(f => path.extname(f)));
}

/**
 * Discover all packages in a sandbox by finding rescript.json files.
 * Returns an array of package directory paths relative to the sandbox root.
 * Skips node_modules and .yarn directories.
 */
const SKIP_DIRS = new Set(["node_modules", ".yarn", "lib"]);

function discoverPackages(sandboxPath) {
  const packages = [];

  function scan(dir) {
    const configPath = path.join(dir, "rescript.json");
    if (existsSync(configPath)) {
      packages.push(path.relative(sandboxPath, dir) || ".");
    }
    for (const entry of readdirSync(dir, { withFileTypes: true })) {
      if (!entry.isDirectory()) continue;
      if (SKIP_DIRS.has(entry.name)) continue;
      scan(path.join(dir, entry.name));
    }
  }

  scan(sandboxPath);
  return packages.sort();
}

/**
 * Read rescript.json sources config and collect all .res/.resi files
 * from the declared source directories.
 * Returns paths relative to the package directory.
 */
function discoverSourceFiles(sandboxPath, packageDir) {
  const configPath = path.join(sandboxPath, packageDir, "rescript.json");
  const config = JSON.parse(readFileSync(configPath, "utf8"));

  // Normalize sources to an array of { dir, subdirs? }
  const sources = Array.isArray(config.sources)
    ? config.sources
    : [config.sources];

  const resFiles = [];
  for (const source of sources) {
    const dir = typeof source === "string" ? source : source.dir;
    const subdirs = typeof source === "object" && source.subdirs === true;
    const absDir = path.join(sandboxPath, packageDir, dir);
    if (!existsSync(absDir)) continue;

    const files = subdirs ? collectFiles(absDir, absDir) : readdirSync(absDir);
    for (const file of files) {
      if (file.endsWith(".res") || file.endsWith(".resi")) {
        resFiles.push(path.join(dir, file));
      }
    }
  }

  return resFiles.sort();
}

describe("lsp", () => {
  it("builds project with lsp profile artifacts on initial build", () =>
    runLspTest(async ({ lsp, sandbox }) => {
      const rootUri = pathToFileURL(sandbox).href;
      await lsp.initialize(rootUri);

      // Wait for the server to finish the initial build
      await lsp.waitForNotification("rescript/buildFinished", 15000);

      // Discover all packages from rescript.json files in the sandbox,
      // then filter to only those in the build's dependency tree (i.e. those
      // that actually have a lib/lsp directory after the build).
      const allPackages = discoverPackages(sandbox);
      const packages = allPackages.filter(packageDir => {
        const lspDir = path.join(sandbox, packageDir, "lib", "lsp");
        return existsSync(lspDir);
      });
      expect(
        packages.length,
        "Expected at least one package with lib/lsp/ after build",
      ).toBeGreaterThan(0);

      for (const packageDir of packages) {
        // Check that .cmi and .cmt files exist (produced by -bs-cmi-only)
        const extensions = getArtifactExtensions(sandbox, packageDir);
        expect(
          extensions.has(".cmi"),
          `Expected .cmi artifacts in ${packageDir}/lib/lsp/`,
        ).toBe(true);
        expect(
          extensions.has(".cmt"),
          `Expected .cmt artifacts in ${packageDir}/lib/lsp/`,
        ).toBe(true);

        // No JS output should be produced
        expect(
          extensions.has(".js"),
          `Expected no .js artifacts in ${packageDir}/lib/lsp/`,
        ).toBe(false);
        expect(
          extensions.has(".mjs"),
          `Expected no .mjs artifacts in ${packageDir}/lib/lsp/`,
        ).toBe(false);
        expect(
          extensions.has(".cmj"),
          `Expected no .cmj artifacts in ${packageDir}/lib/lsp/`,
        ).toBe(false);

        // No source-adjacent JS files should be produced
        const sourceFiles = discoverSourceFiles(sandbox, packageDir);
        for (const sourceFile of sourceFiles) {
          const mjsPath = path.join(
            sandbox,
            packageDir,
            sourceFile.replace(/\.resi?$/, ".mjs"),
          );
          const bsJsPath = path.join(
            sandbox,
            packageDir,
            sourceFile.replace(/\.resi?$/, ".bs.js"),
          );
          expect(
            existsSync(mjsPath),
            `Expected no source-adjacent .mjs for ${packageDir}/${sourceFile}`,
          ).toBe(false);
          expect(
            existsSync(bsJsPath),
            `Expected no source-adjacent .bs.js for ${packageDir}/${sourceFile}`,
          ).toBe(false);
        }
      }
    }));
});
