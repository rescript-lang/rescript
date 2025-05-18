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
async function enforceCompilerMeta({ Yarn }) {
  const EXPECTED_VERSION = "12.0.0-alpha.13";

  for (const workspace of Yarn.workspaces()) {
    const { ident } = workspace.pkg;
    if (ident === "rescript" || ident.startsWith("@rescript/")) {
      workspace.set("version", EXPECTED_VERSION);
      workspace.set("homepage", "https://rescript-lang.org");
      workspace.set("bugs", "https://github.com/rescript-lang/rescript/issues");
      workspace.set("repository", {
        type: "git",
        url: "git+https://github.com/rescript-lang/rescript.git",
      });
      workspace.set("author", {
        name: "Hongbo Zhang",
        email: "bobzhang1988@gmail.com",
      });
      workspace.set("maintainers", [
        "Christoph Knittel (https://github.com/cknitt)",
        "Cristiano Calcagno (https://github.com/cristianoc)",
        "Dmitry Zakharov (https://github.com/DZakh)",
        "Florian Hammerschmidt (https://github.com/fhammerschmidt)",
        "Gabriel Nordeborn (https://github.com/zth)",
        "Hyeseong Kim (https://github.com/cometkim)",
        "Jaap Frolich (https://github.com/jfrolich)",
        "Matthias Le Brun (https://github.com/bloodyowl)",
        "Patrick Ecker (https://github.com/ryyppy)",
        "Paul Tsnobiladzé (https://github.com/tsnobip)",
        "Woonki Moon (https://github.com/mununki)",
      ]);
      workspace.set("preferUnplugged", true);
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
        `compiler/common/bs_version.ml file need to be fixed; expected ${EXPECTED_VERSION}, found ${foundVersion}.`,
      );
    }
  }
}

module.exports = defineConfig({
  async constraints(ctx) {
    await enforceCompilerMeta(ctx);
  },
});
