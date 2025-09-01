// @ts-check
import { createRequire } from "node:module";
import * as path from "node:path";

const require = createRequire(import.meta.url);

const runtimePackageJson = require.resolve("@rescript/runtime/package.json");

export const runtimePath = path.dirname(runtimePackageJson);
