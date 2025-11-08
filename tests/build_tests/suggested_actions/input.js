// @ts-check

import { readdirSync } from "node:fs";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { bsc } = setup(import.meta.dirname);

const expectedDir = path.join(import.meta.dirname, "expected");

const fixtures = readdirSync(path.join(import.meta.dirname, "fixtures")).filter(
  fileName => path.extname(fileName) === ".res",
);

const prefix = ["-w", "+A", "-bs-jsx", "4", "-bs-cmi-only", "-llm-mode"];

const updateTests = process.argv[2] === "update";

/**
 * @param {string} output
 * @return {string}
 */
function postProcessStdout(output) {
  let result = output;
  result = result.trimEnd();
  // Normalize absolute paths in suggested_actions to stable project-relative ones
  result = result.replace(
    /(?:[A-Z]:)?[\\/][^\n]*?tests[\\/]build_tests[\\/]suggested_actions[\\/]/g,
    "tests/build_tests/suggested_actions/",
  );
  return normalizeNewlines(result);
}

let doneTasksCount = 0;
let atLeastOneTaskFailed = false;

for (const fileName of fixtures) {
  const fullFilePath = path.join(import.meta.dirname, "fixtures", fileName);
  const { stdout } = await bsc([...prefix, fullFilePath]);
  doneTasksCount++;

  const actualOutput = postProcessStdout(stdout.toString());
  const expectedFilePath = path.join(expectedDir, `${fileName}.expected`);
  if (updateTests) {
    await fs.mkdir(expectedDir, { recursive: true });
    await fs.writeFile(expectedFilePath, actualOutput);
  } else {
    const expectedOutput = postProcessStdout(
      await fs.readFile(expectedFilePath, "utf-8"),
    );
    if (expectedOutput !== actualOutput) {
      console.error(
        `The old and new stdout for the test ${fullFilePath} aren't the same`,
      );
      console.error("\n=== Old:");
      console.error(expectedOutput);
      console.error("\n=== New:");
      console.error(actualOutput);
      atLeastOneTaskFailed = true;
    }

    if (doneTasksCount === fixtures.length && atLeastOneTaskFailed) {
      process.exit(1);
    }
  }
}
