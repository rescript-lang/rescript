// @ts-check

import { readdirSync } from "node:fs";
import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { bsc } = setup(import.meta.dirname);

const expectedDir = path.join(import.meta.dirname, "expected");

const fixturesInTree = path.join(import.meta.dirname, "fixtures");
const fixtures = readdirSync(fixturesInTree)
  .filter(fileName => path.extname(fileName) === ".res")
  .sort();

const beltLib = path.join(
  import.meta.dirname,
  "..",
  "..",
  "..",
  "packages",
  "@rescript",
  "belt",
  "lib",
  "ocaml",
);

const prefix = ["-w", "+A", "-bs-jsx", "4", "-I", beltLib];

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

/**
 * @param {string} fixturesDir
 * @param {string} fileName
 * @returns {Promise<{ fileName: string, failure: string | null }>}
 */
async function runFixture(fixturesDir, fileName) {
  const fullFilePath = path.join(fixturesDir, fileName);
  const { stderr } = await bsc([...prefix, "-color", "always", fullFilePath]);
  // careful of:
  // - warning test that actually succeeded in compiling (warning's still in stderr, so the code path is shared here)
  // - accidentally succeeding tests (not likely in this context),
  // actual, correctly erroring test case
  const actualErrorOutput = postProcessErrorOutput(stderr.toString());
  const expectedFilePath = path.join(expectedDir, `${fileName}.expected`);
  if (updateTests && fixturesDir === fixturesInTree) {
    await fs.writeFile(expectedFilePath, actualErrorOutput);
    return { fileName, failure: null };
  }
  const expectedErrorOutput = postProcessErrorOutput(
    await fs.readFile(expectedFilePath, "utf-8"),
  );
  if (expectedErrorOutput === actualErrorOutput) {
    return { fileName, failure: null };
  }
  return {
    fileName,
    failure: [
      `The old and new error output for the test ${fullFilePath} aren't the same`,
      "\n=== Old:",
      expectedErrorOutput,
      "\n=== New:",
      actualErrorOutput,
    ].join("\n"),
  };
}

// Diagnostics must not depend on the length of the source path. Every
// fixture also runs from a copy under a temporary directory whose path is
// longer than any checkout path, ending in the same
// tests/build_tests/super_errors/fixtures suffix so that
// postProcessErrorOutput yields the same snapshot text.
const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), "super_errors-"));
const fixturesLongPath = path.join(
  tempRoot,
  "x".repeat(200),
  "tests",
  "build_tests",
  "super_errors",
  "fixtures",
);
await fs.mkdir(fixturesLongPath, { recursive: true });
for (const fileName of fixtures) {
  await fs.copyFile(
    path.join(fixturesInTree, fileName),
    path.join(fixturesLongPath, fileName),
  );
}

/** @type {Array<[string, string]>} */
const runs = [];
for (const fixturesDir of [fixturesInTree, fixturesLongPath]) {
  for (const fileName of fixtures) runs.push([fixturesDir, fileName]);
}

// Run fixtures in parallel with a worker-pool. Each fixture spawns a bsc
// process, so wall time is dominated by process startup; serialising the
// loop made the suite scale linearly with fixture count.
const concurrency = Math.max(1, os.availableParallelism());
let cursor = 0;
const results = new Array(runs.length);

await Promise.all(
  Array.from({ length: Math.min(concurrency, runs.length) }, async () => {
    while (true) {
      const i = cursor++;
      if (i >= runs.length) return;
      results[i] = await runFixture(...runs[i]);
    }
  }),
);
await fs.rm(tempRoot, { recursive: true, force: true });

let atLeastOneTaskFailed = false;
for (const { failure } of results) {
  if (failure !== null) {
    console.error(failure);
    atLeastOneTaskFailed = true;
  }
}
if (atLeastOneTaskFailed) {
  process.exit(1);
}
