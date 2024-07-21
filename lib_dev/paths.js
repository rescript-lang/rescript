// @ts-check

const path = require("node:path");

/**
 * The project root path
 */
const projectDir = path.resolve(__dirname, "..");

/**
 * path: `<projectDir>/compiler/`
 */
const compilerRootDir = path.resolve(projectDir, "compiler");

/**
 * path: `<projectDir>/runtime/`
 */
const runtimeDir = path.resolve(projectDir, "runtime");

/**
 * path: `<projectDir>/lib/js/`
 */
const runtimeCjsOutputDir = path.resolve(projectDir, "lib", "js");

/**
 * path: `<projectDir>/lib/es6/`
 */
const runtimeEsmOutputDir = path.resolve(projectDir, "lib", "es6");

/**
 * path: `<projectDir>/rewatch/`
 */
const rewatchDir = path.resolve(projectDir, "rewatch");

/**
 * path: `<projectDir>/ninja/`
 */
const ninjaDir = path.resolve(projectDir, "ninja");

/**
 * path: `<projectDir>/tests/`
 */
const testDir = path.resolve(projectDir, "tests");

/**
 * path: `<projectDir>/tests/tests/`
 */
const compilerTestDir = path.resolve(testDir, "tests");

/**
 * path: `<projectDir>/tests/build_tests/`
 */
const buildTestDir = path.resolve(testDir, "build_tests");

/**
 * path: `<projectDir>/tests/docstring_tests/`
 */
const docstringTestDir = path.resolve(testDir, "docstring_tests");

/**
 * path: `<projectDir>/compiler/common/bs_version.ml`
 */
const compilerVersionFile = path.resolve(
  compilerRootDir,
  "common",
  "bs_version.ml",
);

/**
 * path: `<projectDir>/_build/install/default/bin/`
 */
const compilerBinDir = path.resolve(
  projectDir,
  "_build",
  "install",
  "default",
  "bin",
);

/**
 * path: `<projectDir>/tests/gentype_tests/typescript-react-example/`
 */
const gentypeExampleDir = path.resolve(
  testDir,
  "gentype_tests",
  "typescript-react-example",
);

module.exports = {
  projectDir,
  compilerRootDir,
  runtimeDir,
  runtimeCjsOutputDir,
  runtimeEsmOutputDir,
  rewatchDir,
  ninjaDir,
  testDir,
  compilerTestDir,
  buildTestDir,
  docstringTestDir,
  compilerVersionFile,
  compilerBinDir,
  gentypeExampleDir,
};
