// @ts-check

import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);

await execBuildOrThrow();

await fs.access(
  path.join(import.meta.dirname, "lib", "bs", "src", "📕annotation", "a.js"),
);
await execClean();
