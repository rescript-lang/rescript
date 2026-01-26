import path from "node:path";
import { fileURLToPath } from "node:url";

const projectRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../../..",
);

const target = `${process.platform}-${process.arch}`;
const binDir = path.join(projectRoot, "packages", "@rescript", target, "bin");

export const rescript_exe = path.join(binDir, "rescript.exe");
export const bsc_exe = path.join(binDir, "bsc.exe");
export const runtimePath = path.join(
  projectRoot,
  "packages",
  "@rescript",
  "runtime",
);
