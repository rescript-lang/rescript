// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { projectDir } from "#dev/paths";
import { setup } from "#dev/process";

const root = import.meta.dirname;
const exporter = path.join(root, "node_modules", "inline-exporter");
const source = path.join(exporter, "src", "Exporter.res");
const output = path.join(root, "src", "B.js");
const inlineArtifact = path.join(
  exporter,
  "lib",
  "bs",
  "src",
  "Exporter.cmj.inline",
);
const original = await fs.readFile(source, "utf8");
const rootBuild = setup(root);
const exporterBuild = setup(exporter);
const exporterOptions = {
  env: {
    ...process.env,
    RESCRIPT_RUNTIME: path.join(projectDir, "packages/@rescript/runtime"),
  },
};

await rootBuild.execClean();
try {
  await exporterBuild.execBuildOrThrow([], exporterOptions);
  await rootBuild.execBuildOrThrow();
  assert.match(await fs.readFile(output, "utf8"), /let result = 2;/);
  await assert.rejects(fs.access(inlineArtifact));

  await fs.writeFile(source, original.replace("x + 1", "x + 2"));
  await exporterBuild.execBuildOrThrow([], exporterOptions);
  await rootBuild.execBuildOrThrow();
  assert.match(await fs.readFile(output, "utf8"), /let result = 3;/);

  await fs.writeFile(source, original.replace("@inline(crossModule)\n", ""));
  await exporterBuild.execBuildOrThrow([], exporterOptions);
  await rootBuild.execBuildOrThrow();
  assert.match(await fs.readFile(output, "utf8"), /Exporter\.bump\(1\)/);
} finally {
  await fs.writeFile(source, original);
  await rootBuild.execClean();
}
