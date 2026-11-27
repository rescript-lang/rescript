import * as fs from "node:fs";
import * as path from "node:path";

const jsFile = process.argv[2];
const logFile = path.join(import.meta.dirname, "post-build-paths.txt");
fs.appendFileSync(logFile, jsFile + "\n");
