// @ts-check
import * as path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const rescriptDir = path.dirname(path.dirname(__dirname));

export const stdlibDir = path.join(rescriptDir, "lib", "ocaml");
