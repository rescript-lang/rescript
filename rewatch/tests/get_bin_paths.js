// @ts-check
import { bsc_exe } from "../../cli/common/bins.js";
import { runtimePath } from "../../cli/common/runtime.js";

console.log(`RESCRIPT_BSC_EXE='${bsc_exe}'`);
console.log(`RESCRIPT_RUNTIME='${runtimePath}'`);
