import fs from "node:fs/promises";
import { join } from "node:path";
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

// Tests js-post-build command execution: when a package has js-post-build
// configured, the daemon should execute the command after each JS file is
// compiled. The build.js_post_build span captures command and js_file.

describe("js-post-build", () => {
  it("executes js-post-build command after compilation", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      const postBuildDir = join(sandbox, "packages", "with-post-build");

      // Build only the with-post-build package
      await build(postBuildDir);
    }));

  it("executes js-post-build with custom command after config change", () =>
    runDaemonTest(async ({ sandbox, build, clean }) => {
      const postBuildDir = join(sandbox, "packages", "with-post-build");

      // Create a script that writes to stderr
      const stderrScript = join(postBuildDir, "post-build-stderr.mjs");
      await fs.writeFile(
        stderrScript,
        `const jsFile = process.argv[2];
console.error(\`stderr-output: \${jsFile}\`);
`,
      );

      // Modify the rescript.json to use the stderr script
      const configPath = join(postBuildDir, "rescript.json");
      const config = JSON.parse(await fs.readFile(configPath, "utf-8"));
      config["js-post-build"] = { cmd: "node post-build-stderr.mjs" };
      await fs.writeFile(configPath, JSON.stringify(config, null, 2));

      // Clean and rebuild
      await clean(postBuildDir);
      await build(postBuildDir);
    }));
});
