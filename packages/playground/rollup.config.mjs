import * as path from "node:path";
import nodeResolve from "@rollup/plugin-node-resolve";
import { glob } from "node:fs/promises";

const RESCRIPT_COMPILER_ROOT_DIR = path.join(import.meta.dirname, "..", "..");
const LIB_DIR = path.join(RESCRIPT_COMPILER_ROOT_DIR, "packages", "@rescript", "runtime", "lib");

// Final target output directory where all the cmijs will be stored
const PACKAGES_DIR = path.join(import.meta.dirname, "packages");
const outputFolder = path.join(PACKAGES_DIR, "compiler-builtins", "stdlib");

let entryPoint = null;

for await (const file of glob(`${LIB_DIR}/es6/*.js`, { withFileTypes: false })) {
  entryPoint = file;
}

if (!entryPoint) {
  throw new Error("No entry point found in playground");
}

export default {
  input: entryPoint,
  output: {
    dir: outputFolder,
    format: "esm",
  },
  plugins: [nodeResolve({ browser: true })],
};
