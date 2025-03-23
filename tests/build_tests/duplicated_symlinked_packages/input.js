// @ts-check

import * as fs from "node:fs/promises";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import { setupWithUrl } from "#dev/process";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const { execBuild, execClean } = setupWithUrl(import.meta.url);

const expectedFilePath = "./out.expected";

const updateTests = process.argv[2] === "update";

/**
 * @param {string} output
 * @return {string}
 */
function postProcessErrorOutput(output) {
  return output.trimEnd().replace(new RegExp(__dirname, "gi"), ".");
}

if (process.platform === "win32") {
  console.log("Skipping test on Windows");
  process.exit(0);
}

await execClean();
const { stderr } = await execBuild();

const actualErrorOutput = postProcessErrorOutput(stderr.toString());
if (updateTests) {
  await fs.writeFile(expectedFilePath, actualErrorOutput);
} else {
  const expectedErrorOutput = postProcessErrorOutput(
    await fs.readFile(expectedFilePath, { encoding: "utf-8" }),
  );
  if (expectedErrorOutput !== actualErrorOutput) {
    console.error(`The old and new error output aren't the same`);
    console.error("\n=== Old:");
    console.error(expectedErrorOutput);
    console.error("\n=== New:");
    console.error(actualErrorOutput);
    process.exit(1);
  }
}
