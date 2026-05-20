// @ts-check

import { readdirSync, statSync } from "node:fs";
import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { bsc } = setup(import.meta.dirname);

const fixturesDir = path.join(import.meta.dirname, "fixtures");
const expectedDir = path.join(import.meta.dirname, "expected");

const fixtures = readdirSync(fixturesDir)
  .filter(name => statSync(path.join(fixturesDir, name)).isDirectory())
  .sort();

// `-bs-cmi-only` stops the pipeline after typechecking. Without it, bsc tries
// to emit .mjs and resolve sibling .js files for cross-module imports, which
// fails in this harness because we don't run the build system.
const bscFlags = [
  "-w",
  "+A",
  "-bs-jsx",
  "4",
  "-color",
  "always",
  "-bs-cmi-only",
];

const updateTests = process.argv[2] === "update";

/**
 * @param {string} fixtureName
 * @param {string} output
 * @return {string}
 */
function postProcessErrorOutput(fixtureName, output) {
  let result = output;
  result = result.trimEnd();
  result = result.replace(
    new RegExp(
      `(?:[A-Z]:)?[\\\\/][^ ]+?tests[\\\\/]build_tests[\\\\/]super_errors_multi[\\\\/]fixtures[\\\\/]${fixtureName}[\\\\/]([^:\\s]+)`,
      "g",
    ),
    (_match, file) =>
      `/.../fixtures/${fixtureName}/${file.replace(/\\/g, "/")}`,
  );
  return normalizeNewlines(result);
}

/**
 * Per-fixture artifact extensions to clean between runs so each invocation
 * compiles against a known-empty .cmi/.cmj/.cmt set.
 */
const artifactExts = [".cmi", ".cmj", ".cmt", ".cmti", ".mjs", ".js"];

/**
 * @param {string} dir
 */
async function cleanArtifacts(dir) {
  const entries = await fs.readdir(dir);
  await Promise.all(
    entries.map(async name => {
      if (artifactExts.includes(path.extname(name))) {
        await fs.unlink(path.join(dir, name));
      }
    }),
  );
}

/**
 * @param {string} fixtureName
 * @returns {Promise<{ fixtureName: string, failure: string | null }>}
 */
async function runFixture(fixtureName) {
  const fixtureDir = path.join(fixturesDir, fixtureName);

  // Compile sources in alphabetical order, with one tweak: for any module
  // that has both `.resi` and `.res`, the interface goes first so the
  // implementation type-checks against the produced `.cmi`. Fixtures that
  // need a specific dependency order across modules can prefix filenames
  // with numeric labels (e.g. `01_Foo.res`, `02_Bar.res`).
  const sources = (await fs.readdir(fixtureDir))
    .filter(name => name.endsWith(".res") || name.endsWith(".resi"))
    .sort((a, b) => {
      const aStem = a.replace(/\.resi?$/, "");
      const bStem = b.replace(/\.resi?$/, "");
      if (aStem === bStem) {
        return a.endsWith(".resi") ? -1 : 1;
      }
      return aStem < bStem ? -1 : aStem > bStem ? 1 : 0;
    });

  await cleanArtifacts(fixtureDir);

  // Modules that have both an interface and an implementation need
  // `-bs-read-cmi` on the implementation so the `.cmi` produced from the
  // interface is enforced rather than overwritten.
  const hasInterface = new Set();
  for (const source of sources) {
    if (source.endsWith(".resi")) hasInterface.add(source.slice(0, -5));
  }

  const chunks = [];
  for (const source of sources) {
    const stem = source.replace(/\.resi?$/, "");
    const extraFlags =
      source.endsWith(".res") && hasInterface.has(stem) ? ["-bs-read-cmi"] : [];
    const { stderr } = await bsc(
      [
        ...bscFlags,
        ...extraFlags,
        "-I",
        fixtureDir,
        path.join(fixtureDir, source),
      ],
      { cwd: fixtureDir },
    );
    const stderrStr = stderr.toString();
    if (stderrStr.length > 0) {
      // Tag each chunk so a reader can tell which file produced which
      // diagnostic — important when the same error gets re-emitted in the
      // downstream consumer.
      chunks.push(`===== ${source} =====\n${stderrStr}`);
    }
  }

  const actualErrorOutput = postProcessErrorOutput(
    fixtureName,
    chunks.join("\n"),
  );
  const expectedFilePath = path.join(expectedDir, `${fixtureName}.expected`);

  if (updateTests) {
    await fs.writeFile(expectedFilePath, actualErrorOutput);
    return { fixtureName, failure: null };
  }

  let expectedErrorOutput;
  try {
    expectedErrorOutput = postProcessErrorOutput(
      fixtureName,
      await fs.readFile(expectedFilePath, "utf-8"),
    );
  } catch {
    return {
      fixtureName,
      failure: `Missing expected snapshot for ${fixtureName} (run with 'update' to create)`,
    };
  }

  if (expectedErrorOutput === actualErrorOutput) {
    return { fixtureName, failure: null };
  }
  return {
    fixtureName,
    failure: [
      `The old and new error output for fixture ${fixtureName} aren't the same`,
      "\n=== Old:",
      expectedErrorOutput,
      "\n=== New:",
      actualErrorOutput,
    ].join("\n"),
  };
}

const concurrency = Math.max(1, os.availableParallelism());
let cursor = 0;
const results = new Array(fixtures.length);

await Promise.all(
  Array.from({ length: Math.min(concurrency, fixtures.length) }, async () => {
    while (true) {
      const i = cursor++;
      if (i >= fixtures.length) return;
      results[i] = await runFixture(fixtures[i]);
    }
  }),
);

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
