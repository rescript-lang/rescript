#!/usr/bin/env node

// @ts-check

// Allocation benchmarks: compare how much memory the compiler allocates on a
// fixed set of inputs, before and after a change.
//
// Wall-clock and CPU time are too noisy on shared CI runners to alert on, so
// these benchmarks count the words the compiler allocates, which the OCaml
// runtime reports at exit (OCAMLRUNPARAM=v=0x400). The count is the same in
// every run of the same binary on the same input, and changes by a few words
// with the length of the paths involved. The peak heap size is reported too,
// but isn't usable: it depends on when collections happen, and a few more
// words allocated move it by several percent.
//
// Allocation misses slowdowns that don't allocate, such as a scan of a list
// that already exists, but catches most regressions in the parser, the type
// checker and code generation.
//
//   node scripts/allocBenchmarks.js prepare <checkout>
//   node scripts/allocBenchmarks.js measure <checkout> <out.json>
//   node scripts/allocBenchmarks.js compare <base.json> <head.json>
//     [--threshold <percent>] [--summary <file.md>]
//
// `prepare` builds the runtime and Belt of a checkout whose compiler is
// built. `measure` runs the binaries of a prepared checkout: it replays the
// build of every runtime and Belt source file, one file at a time, with the
// arguments `rescript compiler-args` gives for it, compiles a few generated
// inputs that stress parts of the compiler that were superlinear at some
// point, and parses and formats the large files in tests/alloc_benchmarks/data
// (the build never runs the formatter). The generated inputs and the data
// files come from the checkout of this script, so both measurements use the
// same ones. `compare` reports the changes between two measurements as
// Markdown, improvements and regressions alike, and exits with 1 when a count
// grew by more than the threshold. Runtime and Belt files, whose sources the
// two checkouts may not share, are reported but not alerted on when any of
// them differs.

import { execFileSync, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { binDir } from "#cli/bins";

const repoRoot = path.join(import.meta.dirname, "..");

/** The platform binaries' directory, relative to a checkout's root. */
const relativeBinDir = path.relative(repoRoot, binDir);

/** Large files to parse and format, from the checkout of this script. */
const dataDir = path.join(repoRoot, "tests", "alloc_benchmarks", "data");

/** The packages whose sources are replayed, with their source directories. */
const packages = [
  { dir: "packages/@rescript/runtime", sources: "." },
  { dir: "packages/@rescript/belt", sources: "src" },
];

/**
 * `inputs` is a hash of everything besides the compiler that the counts
 * depend on.
 *
 * @typedef {{ allocated: number, inputs: string }} Measurement
 * @typedef {Record<string, Measurement>} Results
 */

/**
 * Reads the allocated words that OCAMLRUNPARAM=v=0x400 prints to stderr at
 * exit.
 *
 * @param {string} stderr
 * @returns {number | undefined}
 */
export function parseAllocatedWords(stderr) {
  const match = /^allocated_words: (\d+)$/m.exec(stderr);
  return match ? Number(match[1]) : undefined;
}

/**
 * Runs an OCaml executable and returns the words it allocated.
 *
 * @param {string} exe
 * @param {string[]} args
 * @param {string} cwd
 */
function measureRun(exe, args, cwd) {
  const result = spawnSync(exe, args, {
    cwd,
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
    env: { ...process.env, OCAMLRUNPARAM: "v=0x400" },
  });
  const allocated = parseAllocatedWords(result.stderr ?? "");
  if (result.status !== 0 || allocated === undefined) {
    throw new Error(
      `${exe} ${args.join(" ")} failed in ${cwd} (exit ${result.status}):\n${result.stderr}`,
    );
  }
  return allocated;
}

/**
 * A short hash of the files' names and contents.
 *
 * @param {string[]} files
 */
function hashFiles(files) {
  const hash = createHash("sha256");
  for (const file of files) {
    hash.update(path.basename(file)).update("\0");
    hash.update(fs.readFileSync(file)).update("\0");
  }
  return hash.digest("hex").slice(0, 16);
}

/**
 * Generated inputs, one for each part of the compiler that was superlinear
 * at some point. They are small enough to compile in well under a second.
 *
 * @type {Record<string, () => string>}
 */
export const stressInputs = {
  // Exhaustiveness and unused-case checks, and the match compiler.
  IntSwitch: () => {
    const cases = Array.from(
      { length: 1500 },
      (_, i) => `  | ${i} => ${i * 7}`,
    );
    return `let f = x =>\n  switch x {\n${cases.join("\n")}\n  | _ => -1\n  }\n`;
  },
  PolyVariantSwitch: () => {
    const tags = Array.from({ length: 400 }, (_, i) => `#Tag${i}`);
    const cases = tags.map((tag, i) => `  | ${tag} => ${i}`);
    return `type t = [${tags.join(" | ")}]\nlet f = (x: t) =>\n  switch x {\n${cases.join("\n")}\n  }\n`;
  },
  // Pattern variables of a let rec, and the Lambda SCC of its bindings.
  LetRec: () => {
    const bindings = Array.from(
      { length: 2000 },
      (_, i) =>
        `and f${i} = x => if x <= 0 { 0 } else { f${(i + 1) % 2000}(x - 1) + ${i} }`,
    );
    return `let rec start = x => f0(x)\n${bindings.join("\n")}\n`;
  },
  // Label disambiguation and record spreads.
  Record: () => {
    const fields = Array.from({ length: 300 }, (_, i) => `f${i}`);
    return [
      `type t = {${fields.map(f => `${f}: int`).join(", ")}}`,
      `let r = {${fields.map((f, i) => `${f}: ${i}`).join(", ")}}`,
      ...fields.map((f, i) => `let r${i} = {...r, ${f}: r.${f} + 1}`),
      "",
    ].join("\n");
  },
  // The JavaScript printer's output for nested constructors.
  ListLiteral: () => {
    const items = Array.from({ length: 3000 }, (_, i) => `${i}`);
    return `let l = list{${items.join(", ")}}\n`;
  },
  // Inlining of functions whose bodies were inlined already.
  InlineChain: () => {
    const fns = Array.from(
      { length: 300 },
      (_, i) => `let f${i + 1} = x => f${i}(x) + ${i + 1}`,
    );
    return `let f0 = x => x\n${fns.join("\n")}\n`;
  },
};

/**
 * Builds the runtime and Belt of a checkout whose compiler is built, with
 * that checkout's build system. A checkout made with `git worktree add` has
 * no node_modules; the build system only needs to find @rescript/runtime.
 *
 * @param {string} checkout
 */
export function prepare(checkout) {
  const scope = path.join(checkout, "node_modules", "@rescript");
  const runtimeLink = path.join(scope, "runtime");
  if (!fs.existsSync(runtimeLink)) {
    fs.mkdirSync(scope, { recursive: true });
    fs.symlinkSync(
      path.join("..", "..", "packages", "@rescript", "runtime"),
      runtimeLink,
      "junction",
    );
  }
  const rescript = path.join(checkout, relativeBinDir, "rescript.exe");
  for (const pkg of packages) {
    execFileSync(rescript, ["build"], {
      cwd: path.join(checkout, pkg.dir),
      stdio: "inherit",
    });
  }
}

/**
 * @param {string} checkout
 * @returns {Results}
 */
export function measure(checkout) {
  const bins = path.join(checkout, relativeBinDir);
  const bsc = path.join(bins, "bsc.exe");
  const rescript = path.join(bins, "rescript.exe");

  /** @type {Results} */
  const results = {};

  const packageSources = packages.flatMap(pkg => {
    const sourceDir = path.join(checkout, pkg.dir, pkg.sources);
    return fs
      .readdirSync(sourceDir)
      .filter(file => file.endsWith(".res") || file.endsWith(".resi"))
      .sort()
      .map(file => ({ pkg, file, sourcePath: path.join(sourceDir, file) }));
  });
  // Each file is compiled against the interfaces of the others, so a change
  // to any of them can change what compiling it allocates.
  const packageInputs = hashFiles(packageSources.map(s => s.sourcePath));

  for (const { pkg, file, sourcePath } of packageSources) {
    const { parser_args, compiler_args } = JSON.parse(
      execFileSync(rescript, ["compiler-args", sourcePath], {
        encoding: "utf8",
      }),
    );
    // The build parses into lib/rescript, then compiles the AST there. The
    // outputs these runs write are the ones the build wrote already.
    const libDir = path.join(checkout, pkg.dir, "lib", "rescript");
    results[`${pkg.dir}/${path.posix.join(pkg.sources, file)}`] = {
      allocated:
        measureRun(bsc, parser_args, libDir) +
        measureRun(bsc, compiler_args, libDir),
      inputs: packageInputs,
    };
  }

  // The generated inputs and the data files are compiled in a directory of
  // their own under relative names, so that no path of the checkout enters
  // their counts.
  const workDir = fs.mkdtempSync(path.join(os.tmpdir(), "alloc-bench-"));
  try {
    const runtime = path.join(checkout, "packages", "@rescript", "runtime");
    for (const [name, generate] of Object.entries(stressInputs)) {
      const file = path.join(workDir, `${name}.res`);
      fs.writeFileSync(file, generate());
      results[`stress/${name}.res`] = {
        allocated: measureRun(
          bsc,
          ["-runtime-path", runtime, "-bs-no-version-header", `${name}.res`],
          workDir,
        ),
        inputs: hashFiles([file]),
      };
    }

    for (const name of fs.readdirSync(dataDir).sort()) {
      const file = path.join(workDir, name);
      fs.copyFileSync(path.join(dataDir, name), file);
      const inputs = hashFiles([file]);
      results[`parse/${name}`] = {
        allocated: measureRun(
          bsc,
          ["-bs-ast", "-o", `${path.parse(name).name}.ast`, name],
          workDir,
        ),
        inputs,
      };
      results[`format/${name}`] = {
        allocated: measureRun(bsc, ["-format", name], workDir),
        inputs,
      };
    }
  } finally {
    fs.rmSync(workDir, { recursive: true, force: true });
  }

  return results;
}

/**
 * @typedef {{
 *   name: string,
 *   base: number,
 *   head: number,
 *   percent: number,
 *   inputsChanged: boolean,
 * }} Change
 */

/**
 * Lists the counts that differ between two measurements, largest relative
 * change first, and the benchmarks that only one of them has.
 *
 * @param {Results} base
 * @param {Results} head
 * @param {number} threshold percent above which a growth is a regression
 */
export function compareResults(base, head, threshold) {
  /** @type {Change[]} */
  const changes = [];
  for (const [name, after] of Object.entries(head)) {
    const before = base[name];
    if (!before || before.allocated === after.allocated) continue;
    const percent =
      before.allocated === 0
        ? Infinity
        : ((after.allocated - before.allocated) / before.allocated) * 100;
    changes.push({
      name,
      base: before.allocated,
      head: after.allocated,
      percent,
      inputsChanged: before.inputs !== after.inputs,
    });
  }
  changes.sort((x, y) => Math.abs(y.percent) - Math.abs(x.percent));

  const common = Object.keys(head).filter(name => name in base);
  /** @param {Results} results */
  const total = results =>
    common.reduce((sum, name) => sum + results[name].allocated, 0);

  return {
    changes,
    regressions: changes.filter(c => !c.inputsChanged && c.percent > threshold),
    improvements: changes.filter(
      c => !c.inputsChanged && c.percent < -threshold,
    ),
    added: Object.keys(head).filter(name => !(name in base)),
    removed: Object.keys(base).filter(name => !(name in head)),
    compared: common.length,
    total: { base: total(base), head: total(head) },
  };
}

/** @param {number} words */
const formatWords = words => words.toLocaleString("en-US");

/** @param {number} percent */
const formatPercent = percent =>
  Number.isFinite(percent)
    ? `${percent > 0 ? "+" : ""}${percent.toFixed(2)}%`
    : "new";

/** @param {number} n */
const allocate = n =>
  n === 1 ? "1 benchmark allocates" : `${n} benchmarks allocate`;

/**
 * Changes smaller than this are left out of the table: the paths of the two
 * checkouts differ in length, which changes each count by a few words.
 */
const minListedPercent = 0.1;

/**
 * @param {ReturnType<typeof compareResults>} comparison
 * @param {number} threshold
 * @param {number} [visibleRows] the largest changes shown in the table; the
 *   others are in a collapsed section under it
 */
export function renderSummary(comparison, threshold, visibleRows = 10) {
  const {
    changes,
    regressions,
    improvements,
    added,
    removed,
    compared,
    total,
  } = comparison;
  const totalPercent =
    total.base === 0 ? 0 : ((total.head - total.base) / total.base) * 100;
  // A total that rounds to 0.00% would read as "+0.00%" or "-0.00%".
  const totalChange =
    Math.abs(totalPercent) < 0.005 ? "±0.00%" : formatPercent(totalPercent);
  // The title shows the overall status: a regression outweighs an
  // improvement, and changed inputs alone never count as either.
  const status =
    regressions.length > 0 ? "⚠️" : improvements.length > 0 ? "🎉" : "✅";
  const lines = [
    `## ${status} Allocation benchmarks`,
    "",
    `Allocated words in total: ${formatWords(total.base)} → ${formatWords(total.head)} (${totalChange}).`,
    "",
  ];
  if (regressions.length > 0) {
    lines.push(
      `**⚠️ Regression: ${allocate(regressions.length)} more than ${threshold}% more.**`,
      "",
    );
  }
  if (improvements.length > 0) {
    lines.push(
      `**🎉 Improvement: ${allocate(improvements.length)} more than ${threshold}% less.**`,
      "",
    );
  }
  const listed = changes.filter(c => Math.abs(c.percent) >= minListedPercent);
  if (listed.length === 0) {
    lines.push(
      compared === 1
        ? `The benchmark didn't change by ${minListedPercent}% or more.`
        : `None of the ${compared} benchmarks changed by ${minListedPercent}% or more.`,
    );
  } else {
    /** @param {Change[]} rows */
    const table = rows => [
      "| Benchmark | Base | Head | Change |",
      "| --- | ---: | ---: | ---: |",
      ...rows.map(change => {
        const note = change.inputsChanged ? " (inputs changed)" : "";
        return `| \`${change.name}\`${note} | ${formatWords(change.base)} | ${formatWords(change.head)} | ${formatPercent(change.percent)} |`;
      }),
    ];
    lines.push(...table(listed.slice(0, visibleRows)));
    // A change that affects every benchmark lists them all; the largest stay
    // visible, and the others are folded away rather than left out.
    const folded = listed.slice(visibleRows);
    if (folded.length > 0) {
      lines.push(
        "",
        `<details><summary>${folded.length} more ${folded.length === 1 ? "change" : "changes"}</summary>`,
        "",
        ...table(folded),
        "",
        "</details>",
      );
    }
    if (listed.some(c => c.inputsChanged)) {
      lines.push(
        "",
        "(inputs changed): this pull request changes runtime or Belt sources, so these benchmarks compile different code than on the base, and their changes don't count as regressions.",
      );
    }
  }
  if (added.length > 0) lines.push("", `New: ${added.join(", ")}.`);
  if (removed.length > 0) lines.push("", `Removed: ${removed.join(", ")}.`);
  return `${lines.join("\n")}\n`;
}

/** @param {string[]} args */
function main(args) {
  const [command, ...rest] = args;
  if (command === "prepare" && rest.length === 1) {
    prepare(path.resolve(rest[0]));
    return 0;
  }
  if (command === "measure" && rest.length === 2) {
    const [checkout, out] = rest;
    const results = measure(path.resolve(checkout));
    fs.writeFileSync(out, `${JSON.stringify(results, null, 2)}\n`);
    console.log(`Measured ${Object.keys(results).length} benchmarks.`);
    return 0;
  }
  if (command === "compare" && rest.length >= 2) {
    const [basePath, headPath, ...options] = rest;
    let threshold = 1;
    let summaryPath;
    for (let i = 0; i < options.length; i += 2) {
      if (options[i] === "--threshold") threshold = Number(options[i + 1]);
      else if (options[i] === "--summary") summaryPath = options[i + 1];
      else throw new Error(`Unknown option ${options[i]}`);
    }
    const base = JSON.parse(fs.readFileSync(basePath, "utf8"));
    const head = JSON.parse(fs.readFileSync(headPath, "utf8"));
    const comparison = compareResults(base, head, threshold);
    const summary = renderSummary(comparison, threshold);
    console.log(summary);
    if (summaryPath) fs.writeFileSync(summaryPath, summary);
    return comparison.regressions.length > 0 ? 1 : 0;
  }
  console.error(
    "Usage: allocBenchmarks.js prepare <checkout>\n" +
      "       allocBenchmarks.js measure <checkout> <out.json>\n" +
      "       allocBenchmarks.js compare <base.json> <head.json> [--threshold <percent>]\n" +
      "         [--summary <file.md>]",
  );
  return 2;
}

if (import.meta.filename === path.resolve(process.argv[1] ?? "")) {
  process.exitCode = main(process.argv.slice(2));
}
