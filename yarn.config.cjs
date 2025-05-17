// @ts-check

/**
 * @import { Yarn } from "@yarnpkg/types"
 */

const fs = require("node:fs/promises");
const { defineConfig } = require("@yarnpkg/types");

const { compilerVersionFile } = require("#dev/paths");

/**
 * @param {Yarn.Constraints.Context} ctx
 */
async function enforceCompilerVersion({ Yarn }) {
  const EXPECTED_VERSION = "12.0.0-alpha.13";

  for (const workspace of Yarn.workspaces()) {
    const { ident } = workspace.pkg;
    if (ident === "rescript" || ident.startsWith("@rescript/")) {
      workspace.set("version", EXPECTED_VERSION);
    }
  }

  const versionFile = await fs.readFile(compilerVersionFile, "utf8");
  const versionPattern = /^let version = "(?<version>[^"]+)"$/m;

  if (process.argv.includes("--fix")) {
    await fs.writeFile(
      compilerVersionFile,
      versionFile.replace(
        versionPattern,
        `let version = "${EXPECTED_VERSION}"`,
      ),
    );
  } else {
    const versionMatch = versionFile.match(versionPattern);
    const foundVersion = versionMatch?.groups?.version;
    if (foundVersion !== EXPECTED_VERSION) {
      Yarn.workspace().error(
        `compiler/common/bs_version.ml file need to be fiexed; expected ${EXPECTED_VERSION}, found ${foundVersion}.`,
      );
    }
  }
}

module.exports = defineConfig({
  async constraints(ctx) {
    await enforceCompilerVersion(ctx);
  },
});
