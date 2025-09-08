import * as Setup from "../src/Setup.mjs";
import * as Nodeurl from "node:url";
import * as Nodepath from "node:path";
import * as Nodetest from "node:test";

let repo = Nodepath.resolve(
  Nodepath.dirname(Nodeurl.fileURLToPath(import.meta.url)),
  "../repos/npm/single-project"
);
let commands = Setup.commands(repo);
try {
  await commands.npm.install();
} catch (error) {
  console.error(error);
}
