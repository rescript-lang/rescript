// @ts-check

import { readdirSync } from "node:fs";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { bsc, rescriptTools } = setup(import.meta.dirname);

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
  return normalizeNewlines(result);
}

let doneTasksCount = 0;
let atLeastOneTaskFailed = false;

for (const fileName of fixtures) {
  const fullFilePath = path.join(import.meta.dirname, "fixtures", fileName);
  const cmtPath = fullFilePath.replace(".res", ".cmt");
  await bsc([...prefix, "-color", "always", fullFilePath]);
  const { stdout, stderr } = await rescriptTools("actions", [
    fullFilePath,
    cmtPath,
  ]);
  if (stderr.length > 0) {
    console.error(stderr.toString());
  }
  doneTasksCount++;
  const expectedFilePath = path.join(
    expectedDir,
    `${fileName.replace(".res", "")}_applied.res`
  );
  const actualActions = postProcessErrorOutput(stdout.toString());
  if (updateTests) {
    await fs.writeFile(expectedFilePath, actualActions);
  } else {
    const expectedActions = postProcessErrorOutput(
      await fs.readFile(expectedFilePath, "utf-8")
    );
    if (expectedActions !== actualActions) {
      console.error(
        `The old and new actions for the test ${fullFilePath} aren't the same`
      );
      console.error("\n=== Old:");
      console.error(expectedActions);
      console.error("\n=== New:");
      console.error(actualActions);
      atLeastOneTaskFailed = true;
    }

    if (doneTasksCount === fixtures.length && atLeastOneTaskFailed) {
      process.exit(1);
    }
  }
}
