import { test } from "node:test";
import fs from "node:fs/promises";
import path from "node:path";
import { glob } from "glob";
import { spawn, spawnSync } from "node:child_process";

// Get the current ReScript version
const rescriptVersion = spawnSync("./node_modules/.bin/bsc", ["-v"])
  .stdout.toString()
  .trim()
  .replace("ReScript ", "");

const testFilesDir = path.join(import.meta.dirname, "./test_files");
const buildDir = path.join(import.meta.dirname, "./test_files/.build");
const incrementalDir = path.join(
  import.meta.dirname,
  "./lib/bs/___incremental"
);

// Recreate directories needed
try {
  await fs.access(incrementalDir);
  await fs.rm(incrementalDir, { recursive: true });
} catch (_) {}
await fs.mkdir(incrementalDir, { recursive: true });

try {
  await fs.access(buildDir);
  await fs.rm(buildDir, { recursive: true });
} catch (_) {}
await fs.mkdir(buildDir, { recursive: true });

const resFiles = await glob("**/*.res", {
  cwd: testFilesDir,
  absolute: true,
}).then((files) =>
  files.map((file) => ({
    absolutePath: file,
    relativePath: path.relative(testFilesDir, file),
  }))
);

// Function to split test file contents into blocks
const splitTestBlocks = (contents) => {
  const testBlocks = contents.split(/\/\/ == TEST:/);
  // Skip the first empty block if it exists
  return testBlocks.slice(1).map((block) => {
    const [description, ...rest] = block.split("\n");
    return {
      description: description.trim(),
      content: rest.join("\n").trim(),
    };
  });
};

const testBlocksPerFile = new Map();

const baseCommand = `./node_modules/.bin/bsc -I lib/bs/support_files -I node_modules/@rescript/react/lib/ocaml -editor-mode -ignore-parse-errors -color never`;

// Compile all files and move incremental cmt's
await Promise.all(
  resFiles.map(async (file) => {
    const contents = await fs.readFile(file.absolutePath, "utf-8");
    const testBlocks = splitTestBlocks(contents);

    let blockIndex = 1;
    const blockData = [];

    for (const block of testBlocks) {
      const { description, content } = block;
      const filePath = path.join(
        buildDir,
        `${file.relativePath.slice(0, -4)}_${blockIndex}.res`
      );

      const fileContent = `// ${description}\n${content}`;

      await fs.writeFile(filePath, fileContent);

      const command = `${baseCommand} ${filePath}`;
      const [cmd, ...args] = command.split(" ");

      const _debugData = await new Promise((resolve) => {
        const child = spawn(cmd, args);

        let stdout = "";
        let stderr = "";

        child.stdout.on("data", (chunk) => {
          stdout += chunk;
        });

        child.stderr.on("data", (chunk) => {
          stderr += chunk;
        });

        child.on("close", () => {
          resolve({ stdout, stderr });
        });
      });

      // Move .cmt file to incremental directory
      const cmtPath = filePath.replace(".res", ".cmt");
      const cmtFileName = path.basename(cmtPath);
      const targetPath = path.join(incrementalDir, cmtFileName);
      await fs.rename(cmtPath, targetPath);

      blockData.push({ filePath, description, fileContent });
      blockIndex++;
    }

    testBlocksPerFile.set(file.relativePath, blockData);
  })
);

resFiles.forEach((file) => {
  const blockData = testBlocksPerFile.get(file.relativePath);
  for (const block of blockData) {
    test(`${file.relativePath} - ${block.description}`, async (t) => {
      // Run rescript-editor-analysis and capture output
      const analysisOutput = await new Promise((resolve, reject) => {
        const analysisCmd = spawn(
          "../../../_build/install/default/bin/rescript-editor-analysis",
          ["test_revamped", block.filePath, "rescript.test.json"],
          {
            stdio: "pipe",
            env: {
              RESCRIPT_INCREMENTAL_TYPECHECKING: "true",
              RESCRIPT_PROJECT_CONFIG_CACHE: "true",
              RESCRIPT_VERSION: rescriptVersion,
            },
          }
        );

        let stdout = "";
        let stderr = "";

        analysisCmd.stdout.on("data", (data) => {
          stdout += data.toString();
        });

        analysisCmd.stderr.on("data", (data) => {
          stderr += data.toString();
        });

        analysisCmd.on("close", (code) => {
          if (code === 0) {
            resolve({ stdout, stderr });
          } else {
            reject(new Error(`Analysis command failed with code ${code}`));
            console.error(stderr);
          }
        });
      });

      t.assert.snapshot(analysisOutput.stdout);
    });
  }
});
