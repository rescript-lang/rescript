import * as path from "node:path";
import nodeResolve from "@rollup/plugin-node-resolve";
import { glob } from "glob";

const RESCRIPT_COMPILER_ROOT_DIR = path.join(import.meta.dirname, "..", "..");
const LIB_DIR = path.join(RESCRIPT_COMPILER_ROOT_DIR, "lib");
const PLAYGROUND_DIR = path.join(RESCRIPT_COMPILER_ROOT_DIR, "playground");
// Final target output directory where all the cmijs will be stored
const PACKAGES_DIR = path.join(PLAYGROUND_DIR, "packages");
const outputFolder = path.join(PACKAGES_DIR, "compiler-builtins", "stdlib");

const entryPoint = await glob(`${LIB_DIR}/es6/*.js`);
export default {
  input: entryPoint,
  output: {
    dir: outputFolder,
    format: "esm",
  },
  plugins: [nodeResolve({ browser: true })],
};
