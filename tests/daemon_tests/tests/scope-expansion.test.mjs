import { join } from "node:path";
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// The daemon lazily loads packages based on which working_directory a client
// connects from. When a build is requested from a leaf package, only that
// package (and its dependencies) are loaded. A subsequent build from the root
// should expand the daemon's state to include all packages in the monorepo.

describe("scope-expansion", () => {
  // Building from a leaf package should only compile that package's scope.
  // The daemon discovers the monorepo root but only loads the subset of
  // packages reachable from the working directory.
  it("builds only the leaf package when invoked from its directory", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      const libDir = join(sandbox, "packages", "library");
      await build(libDir);
    }));

  // Building from a package that has dependencies (app depends on library)
  // should load both the package and its transitive dependencies. Without this,
  // the compiler cannot find modules from dependency packages.
  it("scoped build loads transitive dependencies", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      const appDir = join(sandbox, "packages", "app");
      await build(appDir);
    }));

  // After a scoped build from a leaf, a root build should load all packages.
  // Note: build RPC always clears sources before building (to reflect current
  // filesystem state), so the second build reloads everything.
  it("root build loads all packages after a scoped leaf build", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      const libDir = join(sandbox, "packages", "library");

      // First: scoped build loads only library
      await build(libDir);

      // Second: root build loads all packages
      await build(sandbox);
    }));
});
