const resolve = require("@rollup/plugin-node-resolve");

const { globSync } = require("glob");
const path = require("path");

const RESCRIPT_COMPILER_ROOT_DIR = path.join(__dirname, "..", "..");
const LIB_DIR = path.join(RESCRIPT_COMPILER_ROOT_DIR, "lib");
const PLAYGROUND_DIR = path.join(RESCRIPT_COMPILER_ROOT_DIR, "playground");
// Final target output directory where all the cmijs will be stored
const PACKAGES_DIR = path.join(PLAYGROUND_DIR, "packages");
const outputFolder = path.join(PACKAGES_DIR, "compiler-builtins", "stdlib");

module.exports = globSync(`${LIB_DIR}/es6/*.js`).map((entryPoint) => {
  return {
    input: entryPoint,
    output: {
      dir: outputFolder,
      format: "esm",
    },
    plugins: [resolve({ browser: true })],
  };
});
