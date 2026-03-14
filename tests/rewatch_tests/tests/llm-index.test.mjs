import child_process from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { bsc_exe, runtimePath } from "../helpers/bins.mjs";
import { createRescriptCli } from "../helpers/process.mjs";
import { createSandbox, removeSandbox } from "../helpers/sandbox.mjs";

const analysisExe = path.join(
  path.dirname(bsc_exe),
  "rescript-editor-analysis.exe",
);

/**
 * Run the analysis binary with args and stdin, return parsed JSON.
 */
function runAnalysis(args, stdinJson, cwd) {
  return new Promise((resolve, reject) => {
    const proc = child_process.spawn(analysisExe, args, {
      cwd,
      stdio: ["pipe", "pipe", "pipe"],
      env: {
        ...process.env,
        RESCRIPT_BSC_EXE: bsc_exe,
        RESCRIPT_RUNTIME: runtimePath,
      },
    });

    const stdoutChunks = [];
    const stderrChunks = [];
    proc.stdout.on("data", chunk => stdoutChunks.push(chunk));
    proc.stderr.on("data", chunk => stderrChunks.push(chunk));

    proc.once("error", reject);
    proc.once("close", () => {
      const stdout = Buffer.concat(stdoutChunks).toString("utf8");
      const stderr = Buffer.concat(stderrChunks).toString("utf8");
      resolve({ stdout, stderr });
    });

    proc.stdin.write(JSON.stringify(stdinJson));
    proc.stdin.end();
  });
}

/**
 * Build the pathsForModule entry for a module.
 */
function moduleEntry(buildDir, moduleName, hasInterface) {
  if (hasInterface) {
    return {
      intfAndImpl: {
        cmti: path.join(buildDir, `${moduleName}.cmti`),
        resi: path.join(buildDir, `${moduleName}.resi`),
        cmt: path.join(buildDir, `${moduleName}.cmt`),
        res: path.join(buildDir, `${moduleName}.res`),
      },
    };
  }
  return {
    impl: {
      cmt: path.join(buildDir, `${moduleName}.cmt`),
      res: path.join(buildDir, `${moduleName}.res`),
    },
  };
}

describe("llmIndex", () => {
  it("extracts module information from the library package", async () => {
    let sandbox;
    try {
      sandbox = await createSandbox();
      const cli = createRescriptCli(sandbox);

      // Build the project first
      const buildResult = await cli.build();
      expect(buildResult.status).toBe(0);

      // Construct stdin JSON for the library package
      const libraryRoot = path.join(sandbox, "packages", "library");
      const buildDir = path.join(libraryRoot, "lib", "bs", "src");

      const stdinJson = {
        packages: [
          {
            rootPath: libraryRoot,
            suffix: ".mjs",
            rescriptVersion: [13, 0],
            genericJsxModule: null,
            opens: [],
            pathsForModule: {
              Library: moduleEntry(buildDir, "Library", false),
              Unrelated: moduleEntry(buildDir, "Unrelated", false),
            },
            projectFiles: ["Library", "Unrelated"],
            dependenciesFiles: [],
            files: [
              {
                moduleName: "Library",
                cmt: path.join(buildDir, "Library.cmt"),
                cmti: "",
              },
            ],
          },
        ],
      };

      const { stdout, stderr } = await runAnalysis(
        ["rewatch", "llmIndex"],
        stdinJson,
        libraryRoot,
      );

      if (stderr) {
        console.error("analysis stderr:", stderr);
      }

      // Parse and re-serialize for stable formatting
      const result = JSON.parse(stdout);

      // Normalize paths: replace sandbox-specific paths with placeholders.
      // On Windows, paths may use backslashes, so normalize both variants.
      const sandboxForward = sandbox.replaceAll("\\", "/");
      let jsonStr = JSON.stringify(result);
      // Normalize backslashes in the JSON to forward slashes first
      jsonStr = jsonStr.replaceAll("\\\\", "/");
      // Then replace the sandbox path (with and without trailing slash)
      jsonStr = jsonStr.replaceAll(sandboxForward + "/", "<SANDBOX>/");
      jsonStr = jsonStr.replaceAll(sandboxForward, "<SANDBOX>");
      const normalized = JSON.parse(jsonStr);

      expect(normalized).toMatchSnapshot();
    } finally {
      if (sandbox) {
        await removeSandbox(sandbox);
      }
    }
  });

  it("rescript sync creates rescript.db with expected data", async () => {
    let sandbox;
    try {
      sandbox = await createSandbox();
      const cli = createRescriptCli(sandbox);

      // Run sync
      const syncResult = await cli.sync();
      expect(syncResult.status).toBe(0);

      // Verify rescript.db was created
      const dbPath = path.join(sandbox, "rescript.db");
      expect(fs.existsSync(dbPath)).toBe(true);

      // Query the database to verify contents
      const query = sql => {
        const result = child_process.execSync(`sqlite3 "${dbPath}" "${sql}"`, {
          encoding: "utf8",
        });
        return result.trim();
      };

      // Should have packages
      const pkgCount = parseInt(query("SELECT COUNT(*) FROM packages"), 10);
      expect(pkgCount).toBeGreaterThan(0);

      // Should have modules
      const modCount = parseInt(query("SELECT COUNT(*) FROM modules"), 10);
      expect(modCount).toBeGreaterThan(0);

      // Should have the Library module with its record type
      const libraryType = query(
        "SELECT name, kind FROM types WHERE name = 'user' AND kind = 'record' LIMIT 1",
      );
      expect(libraryType).toBe("user|record");

      // Should have fields for the user record
      const userField = query(
        "SELECT f.name, f.signature FROM fields f JOIN types t ON f.type_id = t.id WHERE t.name = 'user' LIMIT 1",
      );
      expect(userField).toBe("name|string");

      // Should have values
      const valCount = parseInt(query("SELECT COUNT(*) FROM [values]"), 10);
      expect(valCount).toBeGreaterThan(0);
    } finally {
      if (sandbox) {
        await removeSandbox(sandbox);
      }
    }
  });
});
