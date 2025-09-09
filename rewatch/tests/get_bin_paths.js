// @ts-check
import { bsc_exe } from "../../cli/common/bins.js";
import path from "node:path";

const runtimePath = path.resolve(
  import.meta.dirname,
  "..",
  "..",
  "packages",
  "@rescript",
  "runtime"
);

console.log(`RESCRIPT_BSC_EXE='${bsc_exe}'`);
console.log(`RESCRIPT_RUNTIME='${runtimePath}'`);
