#!/usr/bin/env node

import * as fs from "node:fs/promises";
import * as path from "node:path";

const devPlaygroundDir = path.join(import.meta.dirname, "..");
const distDir = path.join(devPlaygroundDir, "dist");
const siteDir = path.join(devPlaygroundDir, "pages-site");
const sitePath = process.env.GITHUB_PAGES_PATH ?? "dev-playground";
const bundleId = process.env.PLAYGROUND_BUNDLE_ID ?? "master";
const commitSha = process.env.GITHUB_SHA ?? "unknown";
const targetDir = path.join(siteDir, sitePath);

async function assertExists(filePath, message) {
  try {
    await fs.stat(filePath);
  } catch {
    throw new Error(`${message}: ${filePath}`);
  }
}

await assertExists(
  distDir,
  "Missing dev playground build. Run `yarn workspace dev-playground build` first",
);

await fs.rm(siteDir, {recursive: true, force: true});
await fs.mkdir(targetDir, {recursive: true});
await fs.cp(distDir, targetDir, {recursive: true});

const catalog = {
  generatedAt: new Date().toISOString(),
  defaultBundle: bundleId,
  bundles: [
    {
      id: bundleId,
      label: bundleId,
      channel: bundleId,
      commit: commitSha,
      root: `playground-bundles/${bundleId}`,
    },
  ],
};

await fs.writeFile(
  path.join(targetDir, "catalog.json"),
  `${JSON.stringify(catalog, null, 2)}\n`,
);

await fs.writeFile(
  path.join(siteDir, "index.html"),
  `<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta http-equiv="refresh" content="0; url=./${sitePath}/" />
    <title>ReScript Developer Playground</title>
  </head>
  <body>
    <a href="./${sitePath}/">Open ReScript Developer Playground</a>
  </body>
</html>
`,
);

await assertExists(
  path.join(targetDir, "index.html"),
  "Missing deployed dev playground index",
);
await assertExists(
  path.join(targetDir, "playground-bundles", bundleId, "compiler.js"),
  "Missing deployed playground compiler bundle",
);
await assertExists(
  path.join(
    targetDir,
    "playground-bundles",
    bundleId,
    "compiler-builtins",
    "cmij.js",
  ),
  "Missing deployed compiler-builtins cmij bundle",
);

console.log(`Prepared GitHub Pages site at ${siteDir}`);
