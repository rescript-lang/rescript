// @ts-check

import * as path from "node:path";

/**
 * The project root path
 */
export const projectDir = path.resolve(import.meta.dirname, "..");

/**
 * path: `<projectDir>/compiler/`
 */
export const compilerRootDir = path.resolve(projectDir, "compiler");

/**
 * path: `<projectDir>/tests/`
 */
export const testDir = path.resolve(projectDir, "tests");

/**
 * path: `<projectDir>/tests/compiler_tests/`
 */
export const compilerTestDir = path.resolve(testDir, "compiler_tests");

/**
 * path: `<projectDir>/tests/build_tests/projects/`
 */
export const buildTestDir = path.resolve(testDir, "build_tests", "projects");

/**
 * path: `<projectDir>/tests/project_tests/`
 */
export const projectTestDir = path.resolve(testDir, "project_tests");

/**
 * path: `<projectDir>/tests/error_tests/`
 */
export const errorTestDir = path.resolve(testDir, "error_tests");

/**
 * path: `<projectDir>/tests/runtime_tests/docstrings/`
 */
export const docstringTestDir = path.resolve(
  testDir,
  "runtime_tests",
  "docstrings",
);

/**
 * path: `<projectDir>/tests/runtime_tests/commonjs/`
 */
export const commonjsTestDir = path.resolve(
  testDir,
  "runtime_tests",
  "commonjs",
);

/**
 * path: `<projectDir>/compiler/version/rescript_version.ml`
 */
export const compilerVersionFile = path.resolve(
  compilerRootDir,
  "version",
  "rescript_version.ml",
);

/**
 * path: `<projectDir>/packages/artifacts.json`
 */
export const artifactListFile = path.resolve(
  projectDir,
  "packages",
  "artifacts.json",
);

/**
 * path: `<projectDir>/_build/install/default/bin/`
 */
export const compilerBinDir = path.resolve(
  projectDir,
  "_build",
  "install",
  "default",
  "bin",
);

/**
 * path: `<projectDir>/_build/install/default/bin/ounit_tests[.exe]`
 */
export const ounitTestBin = path.join(
  compilerBinDir,
  process.platform === "win32" ? "ounit_tests.exe" : "ounit_tests",
);
