#!/usr/bin/env node

import * as fs from "node:fs/promises";
import * as path from "node:path";

const testrepo = path.join(import.meta.dirname, "..", "testrepo");
const configs = [
  path.join(testrepo, "node_modules", "rescript-nodejs", "rescript.json"),
  path.join(
    testrepo,
    "packages",
    "nohoist",
    "node_modules",
    "rescript-bun",
    "rescript.json",
  ),
];

for (const configPath of configs) {
  const config = JSON.parse(await fs.readFile(configPath, "utf8"));
  const dependencies = config.dependencies ?? [];
  if (!dependencies.includes("@rescript/belt")) {
    config.dependencies = ["@rescript/belt", ...dependencies];
    await fs.writeFile(configPath, `${JSON.stringify(config, null, 2)}\n`);
  }
}
