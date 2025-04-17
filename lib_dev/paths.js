// @ts-check

import * as path from "node:path";

/**
 * The project root path
 */
export const projectDir = path.resolve(import.meta.dirname, "..");

/**
 * For compatibility reasons, if the architecture is x64, omit it from the bin directory name.
 * So we'll have "darwin", "linux" and "win32" for x64 arch,
 * but "darwinarm64" and "linuxarm64" for arm64 arch.
 * Also, we do not have Windows ARM binaries yet. But the x64 binaries do work on Windows 11 ARM.
 * So omit the architecture for Windows, too.
 */
export const platformName =
  process.arch === "x64" || process.platform === "win32"
    ? process.platform
    : process.platform + process.arch;

/**
 * path: `<projectDir>/<platform>` (e.g. linux, darwinarm64)
 */
export const platformDir = path.resolve(projectDir, platformName);

/**
 * path: `<projectDir>/compiler/`
 */
export const compilerRootDir = path.resolve(projectDir, "compiler");

/**
 * path: `<projectDir>/runtime/`
 */
export const runtimeDir = path.resolve(projectDir, "runtime");

/**
 * path: `<projectDir>/lib/js/`
 */
export const runtimeCjsOutputDir = path.resolve(projectDir, "lib", "js");

/**
 * path: `<projectDir>/lib/es6/`
 */
export const runtimeEsmOutputDir = path.resolve(projectDir, "lib", "es6");

/**
 * path: `<projectDir>/rewatch/`
 */
export const rewatchDir = path.resolve(projectDir, "rewatch");

/**
 * path: `<projectDir>/ninja/`
 */
export const ninjaDir = path.resolve(projectDir, "ninja");

/**
 * path: `<projectDir>/tests/`
 */
export const testDir = path.resolve(projectDir, "tests");

/**
 * path: `<projectDir>/tests/tests/`
 */
export const compilerTestDir = path.resolve(testDir, "tests");

/**
 * path: `<projectDir>/tests/build_tests/`
 */
export const buildTestDir = path.resolve(testDir, "build_tests");

/**
 * path: `<projectDir>/tests/docstring_tests/`
 */
export const docstringTestDir = path.resolve(testDir, "docstring_tests");

/**
 * path: `<projectDir>/compiler/common/bs_version.ml`
 */
export const compilerVersionFile = path.resolve(
  compilerRootDir,
  "common",
  "bs_version.ml",
);

/**
 * path: `<projectDir>/packages/artifacts.txt`
 */
export const artifactListFile = path.resolve(
  projectDir,
  "packages",
  "artifacts.txt",
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
 * path: `<projectDir>/_build/install/default/bin/ounit_tests`
 */
export const ounitTestBin = path.join(compilerBinDir, "ounit_tests");

/**
 * path: `<projectDir>/tests/gentype_tests/typescript-react-example/`
 */
export const gentypeExampleDir = path.resolve(
  testDir,
  "gentype_tests",
  "typescript-react-example",
);
