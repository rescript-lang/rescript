// @ts-check

import { readdirSync } from "node:fs";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { bsc } = setup(import.meta.dirname);

const expectedDir = path.join(import.meta.dirname, "expected");

const fixtures = readdirSync(path.join(import.meta.dirname, "fixtures")).filter(
  (fileName) => path.extname(fileName) === ".res"
);

const prefix = ["-w", "+A", "-bs-jsx", "4"];

const updateTests = process.argv[2] === "update";

/**
 * @param {string} output
 * @return {string}
 */
function postProcessErrorOutput(output) {
  let result = output;
  result = result.trimEnd();
  result = result.replace(
    /(?:[A-Z]:)?[\\/][^ ]+?tests[\\/]build_tests[\\/]super_errors[\\/]([^:]+)/g,
    (_match, path, _offset, _string) => "/.../" + path.replace("\\", "/"),
  );
  return normalizeNewlines(result);
}

let doneTasksCount = 0;
let atLeastOneTaskFailed = false;

for (const fileName of fixtures) {
  const fullFilePath = path.join(import.meta.dirname, "fixtures", fileName);
  const { stderr } = await bsc([...prefix, "-color", "always", fullFilePath]);
  doneTasksCount++;
  // careful of:
  // - warning test that actually succeeded in compiling (warning's still in stderr, so the code path is shared here)
  // - accidentally succeeding tests (not likely in this context),
  // actual, correctly erroring test case
  const actualErrorOutput = postProcessErrorOutput(stderr.toString());
  const expectedFilePath = path.join(expectedDir, `${fileName}.expected`);
  if (updateTests) {
    await fs.writeFile(expectedFilePath, actualErrorOutput);
  } else {
    const expectedErrorOutput = postProcessErrorOutput(
      await fs.readFile(expectedFilePath, "utf-8"),
    );
    if (expectedErrorOutput !== actualErrorOutput) {
      console.error(
        `The old and new error output for the test ${fullFilePath} aren't the same`,
      );
      console.error("\n=== Old:");
      console.error(expectedErrorOutput);
      console.error("\n=== New:");
      console.error(actualErrorOutput);
      atLeastOneTaskFailed = true;
    }

    if (doneTasksCount === fixtures.length && atLeastOneTaskFailed) {
      process.exit(1);
    }
  }
}
