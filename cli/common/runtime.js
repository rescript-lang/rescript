// @ts-check
import { fileURLToPath } from "node:url";
import * as path from "node:path";

const runtimePackageJsonUrl = await import.meta.resolve(
  "@rescript/runtime/package.json",
);
const runtimePackageJsonPath = fileURLToPath(runtimePackageJsonUrl);

export const runtimePath = path.dirname(runtimePackageJsonPath);
