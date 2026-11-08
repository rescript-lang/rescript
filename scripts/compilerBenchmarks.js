#!/usr/bin/env node

// @ts-check

// Compiler benchmarks: compare how much memory the compiler allocates, and
// how many instructions it executes, on a fixed set of inputs, before and
// after a change.
//
// Wall-clock and CPU time are too noisy on shared CI runners to alert on, so
// these benchmarks count instead:
//
// - the words the compiler allocates, which the OCaml runtime reports at exit
//   (OCAMLRUNPARAM=v=0x400). The count is the same in every run of the same
//   binary on the same input from the same path; other paths change it by a
//   few words. The peak heap size is reported too, but isn't usable:
//   it depends on when collections happen, and a few more words allocated
//   move it by several percent.
// - the instructions it executes, which cachegrind reports at exit
//   (`valgrind --tool=cachegrind --cache-sim=no`), for a subset of the
//   benchmarks, since valgrind runs a program 20 to 50 times slower. This
//   catches slowdowns that don't allocate, such as a scan of a list that
//   already exists.
// - the sizes of the compiler's outputs (.cmi, .cmti, .cmt and .cmj files,
//   and the playground's compiler), which are exact.
//
//   node scripts/compilerBenchmarks.js prepare <checkout>
//   node scripts/compilerBenchmarks.js measure <checkout> <out.json> [--instructions]
//     [--project <checkout>] [--playground]
//   node scripts/compilerBenchmarks.js compare <base.json> <head.json>
//     [--threshold <percent>] [--summary <file.md>]
//
// `prepare` builds the runtime and Belt of a checkout whose compiler is
// built. `measure` runs the binaries of a prepared checkout: it replays the
// build of every runtime and Belt source file, one file at a time, with the
// arguments `rescript compiler-args` gives for it, compiles a few generated
// inputs that stress parts of the compiler that were superlinear at some
// point, and parses and formats the large files in tests/compiler_benchmarks/data
// (the build never runs the formatter). With `--instructions`, the generated
// inputs, the data files and the largest runtime and Belt files run under
// cachegrind. The generated inputs and the data files come from the checkout
// of this script, so both measurements use the same ones. With `--project`,
// it also builds tests/tests of the given checkout with the build system,
// clean, with nothing to do and after two edits (see measureProjectBuild);
// the builds after the clean one run under cachegrind with `--instructions`.
// With `--playground`, it measures the size of the playground's compiler.
//
// `compare` reports the changes between two measurements as Markdown,
// improvements and regressions alike, one section per count, and exits with
// 1 when a count grew by more than the threshold. Runtime and Belt files,
// whose sources the two checkouts may not share, are reported but not
// alerted on when any of them differs.

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
const dataDir = path.join(repoRoot, "tests", "compiler_benchmarks", "data");

/** The packages whose sources are replayed, with their source directories. */
const packages = [
  { dir: "packages/@rescript/runtime", sources: "." },
  { dir: "packages/@rescript/belt", sources: "src" },
];

/**
 * How many of the largest runtime and Belt sources run under cachegrind.
 * They are chosen by size in the checkout of this script, so that both
 * measurements choose the same files.
 */
const instructionPackageFiles = 15;

/**
 * The minor heap of the runs under cachegrind, in words. A few words more or
 * less allocated, as from paths of different lengths, move the points where
 * the garbage collector runs, and with them how much of a major cycle is
 * done before the program exits: that changed instruction counts by up to 3%
 * between two builds of the same compiler. With a minor heap larger than
 * any benchmark allocates (35M words at most), the collector doesn't run,
 * and the instructions are the compiler's own; the cost of collecting is in
 * the allocated words, which these settings don't change. The heap is
 * mapped only as it's used, but OCaml reserves address space for a minor
 * heap per possible domain (128 by default), more than valgrind can give,
 * so the runs allow only the one domain bsc uses (d=1).
 */
const cachegrindRuntimeParams = "v=0x400,d=1,s=64M";

/**
 * The runtime parameters of the builds under cachegrind: as above, but the
 * build compiles on a worker domain besides its main one (d=2). The worker
 * sets a minor heap of its own, so the collector runs there; both
 * measurements build from the same paths, so it runs at the same points.
 */
const cachegrindBuildRuntimeParams = "v=0x400,d=2,s=64M";

/**
 * `inputs` is a hash of everything besides the compiler that the counts
 * depend on. `instructions` is only measured for some benchmarks, and
 * `bytes`, the size of an output, only for outputs.
 *
 * @typedef {{
 *   allocated?: number,
 *   instructions?: number,
 *   bytes?: number,
 *   inputs: string,
 * }} Measurement
 * @typedef {Record<string, Measurement>} Results
 * @typedef {"allocated" | "instructions" | "bytes"} Metric
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
  if (!match) return undefined;
  // The build system compiles on worker domains, whose allocations the
  // runtime leaves out; it reports them itself when asked to
  // (REWATCH_REPORT_WORKER_ALLOCATIONS).
  const workers = /^worker_allocated_words: (\d+)$/m.exec(stderr);
  return Number(match[1]) + (workers ? Number(workers[1]) : 0);
}

/**
 * Reads the instructions that cachegrind prints to stderr at exit, as in
 * `==1234== I refs:        12,345,678`.
 *
 * @param {string} stderr
 * @returns {number | undefined}
 */
export function parseInstructions(stderr) {
  const match = /^==\d+== I\s+refs:\s+([\d,]+)\s*$/m.exec(stderr);
  return match ? Number(match[1].replaceAll(",", "")) : undefined;
}

/**
 * Runs an OCaml executable and returns the words it allocated and, under
 * cachegrind, the instructions it executed.
 *
 * @param {string} exe
 * @param {string[]} args
 * @param {string} cwd
 * @param {boolean} instructions
 * @param {{ env?: Record<string, string>, runtimeParams?: string }} [options]
 *   extra environment variables, and the runtime parameters under cachegrind
 * @returns {{ allocated: number, instructions?: number }}
 */
function measureRun(
  exe,
  args,
  cwd,
  instructions,
  { env = {}, runtimeParams = cachegrindRuntimeParams } = {},
) {
  const [command, commandArgs] = instructions
    ? [
        "valgrind",
        [
          "--tool=cachegrind",
          "--cache-sim=no",
          "--cachegrind-out-file=/dev/null",
          exe,
          ...args,
        ],
      ]
    : [exe, args];
  const result = spawnSync(command, commandArgs, {
    cwd,
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
    env: {
      ...process.env,
      ...env,
      OCAMLRUNPARAM: instructions ? runtimeParams : "v=0x400",
    },
  });
  const stderr = result.stderr ?? "";
  const allocated = parseAllocatedWords(stderr);
  const executed = instructions ? parseInstructions(stderr) : undefined;
  if (
    result.status !== 0 ||
    allocated === undefined ||
    (instructions && executed === undefined)
  ) {
    const reason = result.error
      ? result.error.message
      : `exit ${result.status}`;
    throw new Error(
      `${command} ${commandArgs.join(" ")} failed in ${cwd} (${reason}):\n${stderr}`,
    );
  }
  return executed === undefined
    ? { allocated }
    : { allocated, instructions: executed };
}

/**
 * Adds up the counts of the runs of one benchmark.
 *
 * @param {{ allocated: number, instructions?: number }[]} runs
 */
function sumRuns(runs) {
  const allocated = runs.reduce((sum, run) => sum + run.allocated, 0);
  if (runs.some(run => run.instructions === undefined)) return { allocated };
  const instructions = runs.reduce(
    (sum, run) => sum + (run.instructions ?? 0),
    0,
  );
  return { allocated, instructions };
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
 * at some point. They are small enough to compile in well under a second,
 * and each allocates less than the minor heap of the runs under cachegrind
 * (see cachegrindRuntimeParams).
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
  // Field access on a large object type: each access instantiates a copy
  // of the whole type. Quadratic in the number of fields.
  ObjectFields: () => {
    const fields = Array.from({ length: 400 }, (_, i) => `f${i}`);
    return [
      `let o = {${fields.map((f, i) => `"${f}": ${i}`).join(", ")}}`,
      ...fields.map((f, i) => `let a${i} = o["${f}"] + 1`),
      "",
    ].join("\n");
  },
  // Expansion of type aliases that refer to the previous one, with the
  // cycle check of each new definition. Cubic in the length of the chain.
  TypeAliasChain: () => {
    const length = 60;
    const aliases = Array.from(
      { length },
      (_, i) => `type t${i + 1}<'a> = t${i}<('a, int)>`,
    );
    return `type t0<'a> = option<'a>\n${aliases.join("\n")}\nlet x: t${length}<string> = None\n`;
  },
  // Many mutable references updated in one loop. Superlinear in their
  // number although the input is linear.
  RefsInLoop: () => {
    const refs = Array.from({ length: 300 }, (_, i) => `r${i}`);
    return [
      "let f = n => {",
      ...refs.map((r, i) => `  let ${r} = ref(${i})`),
      "  for i in 0 to n {",
      ...refs.map((r, i) => `    ${r} := ${r}.contents + i * ${i + 1}`),
      "  }",
      `  ${refs.map(r => `${r}.contents`).join(" + ")}`,
      "}",
      "",
    ].join("\n");
  },
  // Closures nested in closures, each using the variables of all outer ones.
  NestedClosures: () => {
    const depth = 100;
    const lines = ["let f = x0 => {"];
    for (let i = 1; i < depth; i++) {
      const used = Array.from({ length: i }, (_, j) => `x${j}`).join(" + ");
      lines.push(`${"  ".repeat(i)}let g${i} = x${i} => {`);
      lines.push(`${"  ".repeat(i + 1)}let y${i} = ${used} + x${i}`);
    }
    lines.push(`${"  ".repeat(depth)}y${depth - 1}`);
    for (let i = depth - 1; i >= 1; i--) {
      lines.push(`${"  ".repeat(i)}}`);
      lines.push(`${"  ".repeat(i)}g${i}(${i})`);
    }
    lines.push("}", "");
    return lines.join("\n");
  },
};

/**
 * The runtime and Belt sources of a checkout, by package.
 *
 * @param {string} checkout
 */
function packageSources(checkout) {
  return packages.flatMap(pkg => {
    const sourceDir = path.join(checkout, pkg.dir, pkg.sources);
    return fs
      .readdirSync(sourceDir)
      .filter(file => file.endsWith(".res") || file.endsWith(".resi"))
      .sort()
      .map(file => ({
        pkg,
        file,
        name: `${pkg.dir}/${path.posix.join(pkg.sources, file)}`,
        sourcePath: path.join(sourceDir, file),
      }));
  });
}

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
 * The playground's compiler that `make playground-compiler` builds, relative
 * to a checkout's root.
 */
const playgroundCompiler = path.join("packages", "playground", "compiler.js");

/** The interface and artifact files whose sizes are measured. */
const outputExtensions = [".cmi", ".cmti", ".cmt", ".cmj"];

/**
 * The files in a directory and its subdirectories, sorted. Symbolic links
 * aren't followed: the projects link to the runtime, whose outputs are
 * measured on their own.
 *
 * @param {string} dir
 * @returns {string[]}
 */
function listFiles(dir) {
  return fs
    .readdirSync(dir, { withFileTypes: true })
    .sort((a, b) => a.name.localeCompare(b.name))
    .flatMap(entry => {
      const file = path.join(dir, entry.name);
      if (entry.isDirectory()) return listFiles(file);
      return entry.isFile() ? [file] : [];
    });
}

/**
 * The total size of the files in a directory and its subdirectories, by
 * extension.
 *
 * @param {string} dir
 * @returns {Record<string, number>}
 */
function outputSizes(dir) {
  /** @type {Record<string, number>} */
  const sizes = Object.fromEntries(outputExtensions.map(ext => [ext, 0]));
  for (const file of listFiles(dir)) {
    const ext = path.extname(file);
    if (ext in sizes) sizes[ext] += fs.statSync(file).size;
  }
  return sizes;
}

/**
 * Copies the ReScript sources of a directory and its subdirectories, and
 * nothing else: the outputs the tests check in are written by the builds.
 *
 * @param {string} from
 * @param {string} to
 */
function copySources(from, to) {
  fs.cpSync(from, to, {
    recursive: true,
    filter: source =>
      fs.statSync(source).isDirectory() ||
      source.endsWith(".res") ||
      source.endsWith(".resi"),
  });
}

/** The project the build benchmarks build, relative to a checkout. */
const benchmarkProject = path.join("tests", "tests");

/**
 * The module of the project that about 200 others use, and that has no
 * interface file, so that an edit changes its interface.
 */
const sharedModule = path.join("src", "test_utils.res");

/**
 * Builds tests/tests of `projectCheckout` with the build system of
 * `checkout`, on one worker domain so that the counts don't depend on how
 * the work is shared: a clean build, a build with nothing to do, and the
 * builds after an edit of a module that nothing uses and of one that many
 * do. Adds the sizes of the outputs of the clean build.
 *
 * The project is copied to a directory of its own, with the same name in
 * every measurement, so that no path of the checkout enters the counts.
 * Both measurements build the same sources, those of the base, which its
 * compiler builds already; warnings are no errors there, so that a new
 * warning doesn't fail the build.
 *
 * @param {string} checkout
 * @param {string} projectCheckout
 * @param {boolean} instructions
 * @param {Results} results
 */
function measureProjectBuild(checkout, projectCheckout, instructions, results) {
  const rescript = path.join(checkout, relativeBinDir, "rescript.exe");
  const projectDir = path.join(projectCheckout, benchmarkProject);
  const workDir = path.join(os.tmpdir(), "compiler-bench-project");
  fs.rmSync(workDir, { recursive: true, force: true });
  try {
    copySources(path.join(projectDir, "src"), path.join(workDir, "src"));
    const config = JSON.parse(
      fs.readFileSync(path.join(projectDir, "rescript.json"), "utf8"),
    );
    config["compiler-flags"] = (config["compiler-flags"] ?? []).filter(
      (/** @type {string} */ flag) => !flag.startsWith("-warn-error"),
    );
    fs.writeFileSync(
      path.join(workDir, "rescript.json"),
      `${JSON.stringify(config, null, 2)}\n`,
    );
    const scope = path.join(workDir, "node_modules", "@rescript");
    const react = path.join(
      projectCheckout,
      "tests",
      "dependencies",
      "rescript-react",
    );
    copySources(path.join(react, "src"), path.join(scope, "react", "src"));
    fs.copyFileSync(
      path.join(react, "rescript.json"),
      path.join(scope, "react", "rescript.json"),
    );
    fs.symlinkSync(
      path.join(checkout, "packages", "@rescript", "runtime"),
      path.join(scope, "runtime"),
      "junction",
    );

    const sources = listFiles(workDir).filter(
      file => file.endsWith(".res") || file.endsWith(".resi"),
    );
    const inputs = hashFiles(sources);
    // The largest test, which no other module uses.
    const leafModule = sources
      .filter(file => path.dirname(file) === path.join(workDir, "src"))
      .filter(file => file.endsWith("_test.res"))
      .map(file => ({ file, size: fs.statSync(file).size }))
      .sort((a, b) => b.size - a.size || a.file.localeCompare(b.file))[0]?.file;
    if (!leafModule || !fs.existsSync(path.join(workDir, sharedModule))) {
      throw new Error(`The modules to edit are missing from ${projectDir}.`);
    }

    /** @param {boolean} cachegrind */
    const build = cachegrind =>
      measureRun(rescript, ["build"], workDir, cachegrind, {
        env: {
          REWATCH_COMPILER_DOMAINS: "1",
          REWATCH_REPORT_WORKER_ALLOCATIONS: "1",
        },
        runtimeParams: cachegrindBuildRuntimeParams,
      });
    /** @param {string} file */
    const edit = file => fs.appendFileSync(file, "\nlet benchmarkEdit = 1\n");

    // The clean build runs under cachegrind for no measurement: it would
    // take minutes, and the replayed builds above cover its compiler work.
    results["build/clean"] = { ...build(false), inputs };
    const outputs = outputSizes(workDir);
    for (const ext of outputExtensions) {
      results[`output/tests/*${ext}`] = { bytes: outputs[ext], inputs };
    }
    results["build/no-op"] = { ...build(instructions), inputs };
    edit(leafModule);
    results["build/leaf-edit"] = { ...build(instructions), inputs };
    edit(path.join(workDir, sharedModule));
    results["build/shared-edit"] = { ...build(instructions), inputs };
  } finally {
    fs.rmSync(workDir, { recursive: true, force: true });
  }
}

/**
 * @param {string} checkout
 * @param {{ instructions: boolean, project?: string, playground?: boolean }} options
 *   `project` is the checkout whose tests/tests the build benchmarks build;
 *   without it, there are none. `playground` adds the size of the
 *   playground's compiler, which must be built.
 * @returns {Results}
 */
export function measure(checkout, { instructions, project, playground }) {
  const bins = path.join(checkout, relativeBinDir);
  const bsc = path.join(bins, "bsc.exe");
  const rescript = path.join(bins, "rescript.exe");

  /** @type {Results} */
  const results = {};

  const sources = packageSources(checkout);
  // Each file is compiled against the interfaces of the others, so a change
  // to any of them can change what compiling it allocates.
  const packageInputs = hashFiles(sources.map(s => s.sourcePath));
  const underCachegrind = new Set(
    instructions
      ? packageSources(repoRoot)
          .map(s => ({ name: s.name, size: fs.statSync(s.sourcePath).size }))
          .sort((a, b) => b.size - a.size || a.name.localeCompare(b.name))
          .slice(0, instructionPackageFiles)
          .map(s => s.name)
      : [],
  );

  // One file at a time: the runs write the outputs of the build again, and
  // read the outputs of the files they depend on.
  for (const { pkg, name, sourcePath } of sources) {
    const { parser_args, compiler_args } = JSON.parse(
      execFileSync(rescript, ["compiler-args", sourcePath], {
        encoding: "utf8",
      }),
    );
    // The build parses into lib/rescript, then compiles the AST there. The
    // outputs these runs write are the ones the build wrote already.
    const libDir = path.join(checkout, pkg.dir, "lib", "rescript");
    const cachegrind = underCachegrind.has(name);
    results[name] = {
      ...sumRuns([
        measureRun(bsc, parser_args, libDir, cachegrind),
        measureRun(bsc, compiler_args, libDir, cachegrind),
      ]),
      inputs: packageInputs,
    };
  }

  // The generated inputs and the data files are compiled in a directory of
  // their own under relative names, so that no path of the checkout enters
  // their counts. The directory has the same name in every measurement, so
  // that two measurements of the same compiler give the same counts; they
  // can't run at the same time.
  const workDir = path.join(os.tmpdir(), "compiler-bench");
  fs.rmSync(workDir, { recursive: true, force: true });
  fs.mkdirSync(workDir);
  try {
    const runtime = path.join(checkout, "packages", "@rescript", "runtime");
    for (const [name, generate] of Object.entries(stressInputs)) {
      const file = path.join(workDir, `${name}.res`);
      fs.writeFileSync(file, generate());
      results[`stress/${name}.res`] = {
        ...measureRun(
          bsc,
          ["-runtime-path", runtime, "-bs-no-version-header", `${name}.res`],
          workDir,
          instructions,
        ),
        inputs: hashFiles([file]),
      };
    }

    for (const name of fs.readdirSync(dataDir).sort()) {
      const file = path.join(workDir, name);
      fs.copyFileSync(path.join(dataDir, name), file);
      const inputs = hashFiles([file]);
      results[`parse/${name}`] = {
        ...measureRun(
          bsc,
          ["-bs-ast", "-o", `${path.parse(name).name}.ast`, name],
          workDir,
          instructions,
        ),
        inputs,
      };
      results[`format/${name}`] = {
        ...measureRun(bsc, ["-format", name], workDir, instructions),
        inputs,
      };
    }
  } finally {
    fs.rmSync(workDir, { recursive: true, force: true });
  }

  // The outputs of the runtime and Belt, which the replayed builds wrote
  // again.
  for (const pkg of packages) {
    const outputs = outputSizes(
      path.join(checkout, pkg.dir, "lib", "rescript"),
    );
    for (const ext of outputExtensions) {
      results[`output/${path.basename(pkg.dir)}/*${ext}`] = {
        bytes: outputs[ext],
        inputs: packageInputs,
      };
    }
  }

  if (playground) {
    results["output/playground/compiler.js"] = {
      bytes: fs.statSync(path.join(checkout, playgroundCompiler)).size,
      inputs: "",
    };
  }

  if (project) {
    measureProjectBuild(checkout, project, instructions, results);
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
 * Lists the counts of one metric that differ between two measurements,
 * largest relative change first, and the benchmarks that only one of them
 * has. Benchmarks without the metric on either side are left out.
 *
 * @param {Results} base
 * @param {Results} head
 * @param {number} threshold percent above which a growth is a regression
 * @param {Metric} [metric]
 */
export function compareResults(base, head, threshold, metric = "allocated") {
  /** @param {Results} results */
  const withMetric = results =>
    Object.keys(results).filter(name => results[name][metric] !== undefined);
  const baseNames = new Set(withMetric(base));
  const headNames = new Set(withMetric(head));
  const common = [...headNames].filter(name => baseNames.has(name));

  /** @type {Change[]} */
  const changes = [];
  for (const name of common) {
    const before = /** @type {number} */ (base[name][metric]);
    const after = /** @type {number} */ (head[name][metric]);
    if (before === after) continue;
    changes.push({
      name,
      base: before,
      head: after,
      percent: before === 0 ? Infinity : ((after - before) / before) * 100,
      inputsChanged: base[name].inputs !== head[name].inputs,
    });
  }
  changes.sort((x, y) => Math.abs(y.percent) - Math.abs(x.percent));

  /** @param {Results} results */
  const total = results =>
    common.reduce((sum, name) => sum + (results[name][metric] ?? 0), 0);

  return {
    metric,
    changes,
    regressions: changes.filter(c => !c.inputsChanged && c.percent > threshold),
    improvements: changes.filter(
      c => !c.inputsChanged && c.percent < -threshold,
    ),
    added: [...headNames].filter(name => !baseNames.has(name)),
    removed: [...baseNames].filter(name => !headNames.has(name)),
    compared: common.length,
    total: { base: total(base), head: total(head) },
  };
}

/** @param {number} count */
const formatCount = count => count.toLocaleString("en-US");

/** @param {number} percent */
const formatPercent = percent =>
  Number.isFinite(percent)
    ? `${percent > 0 ? "+" : ""}${percent.toFixed(2)}%`
    : "new";

/**
 * The words of a section, by metric.
 *
 * @type {Record<Metric, {
 *   title: string,
 *   more: (n: number, threshold: number) => string,
 *   less: (n: number, threshold: number) => string,
 * }>}
 */
const metricWords = {
  allocated: {
    title: "Allocated words",
    more: (n, threshold) =>
      `${n === 1 ? "1 benchmark allocates" : `${n} benchmarks allocate`} more than ${threshold}% more`,
    less: (n, threshold) =>
      `${n === 1 ? "1 benchmark allocates" : `${n} benchmarks allocate`} more than ${threshold}% less`,
  },
  instructions: {
    title: "Instructions",
    more: (n, threshold) =>
      `${n === 1 ? "1 benchmark executes" : `${n} benchmarks execute`} more than ${threshold}% more instructions`,
    less: (n, threshold) =>
      `${n === 1 ? "1 benchmark executes" : `${n} benchmarks execute`} more than ${threshold}% fewer instructions`,
  },
  bytes: {
    title: "Output size",
    more: (n, threshold) =>
      `${n === 1 ? "1 output is" : `${n} outputs are`} more than ${threshold}% larger`,
    less: (n, threshold) =>
      `${n === 1 ? "1 output is" : `${n} outputs are`} more than ${threshold}% smaller`,
  },
};

/**
 * Changes smaller than this are left out of the table. Measured from the same
 * path, two builds of the same compiler give the same counts, so a change
 * this small is real, but too small to matter for a pull request.
 */
const minListedPercent = 0.1;

/**
 * The status of comparisons: a regression outweighs an improvement, and
 * changed inputs alone never count as either.
 *
 * @param {ReturnType<typeof compareResults>[]} comparisons
 */
const statusOf = comparisons =>
  comparisons.some(c => c.regressions.length > 0)
    ? "⚠️"
    : comparisons.some(c => c.improvements.length > 0)
      ? "🎉"
      : "✅";

/**
 * @param {ReturnType<typeof compareResults>} comparison
 * @param {number} threshold
 * @param {number} visibleRows
 */
function renderSection(comparison, threshold, visibleRows) {
  const {
    metric,
    changes,
    regressions,
    improvements,
    added,
    removed,
    compared,
    total,
  } = comparison;
  const words = metricWords[metric];
  const totalPercent =
    total.base === 0 ? 0 : ((total.head - total.base) / total.base) * 100;
  // A total that rounds to 0.00% would read as "+0.00%" or "-0.00%".
  const totalChange =
    Math.abs(totalPercent) < 0.005 ? "±0.00%" : formatPercent(totalPercent);
  const lines = [
    `### ${words.title}`,
    "",
    `In total: ${formatCount(total.base)} → ${formatCount(total.head)} (${totalChange}).`,
    "",
  ];
  if (regressions.length > 0) {
    lines.push(
      `**⚠️ Regression: ${words.more(regressions.length, threshold)}.**`,
      "",
    );
  }
  if (improvements.length > 0) {
    lines.push(
      `**🎉 Improvement: ${words.less(improvements.length, threshold)}.**`,
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
        return `| \`${change.name}\`${note} | ${formatCount(change.base)} | ${formatCount(change.head)} | ${formatPercent(change.percent)} |`;
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
  return lines;
}

/**
 * Renders the comparisons as one report, a section for each metric that
 * either measurement has.
 *
 * @param {ReturnType<typeof compareResults>[]} comparisons
 * @param {number} threshold
 * @param {number} [visibleRows] the largest changes shown in each table; the
 *   others are in a collapsed section under it
 */
export function renderSummary(comparisons, threshold, visibleRows = 10) {
  const shown = comparisons.filter(
    c => c.compared > 0 || c.added.length > 0 || c.removed.length > 0,
  );
  const lines = [`## ${statusOf(shown)} Compiler benchmarks`];
  for (const comparison of shown) {
    lines.push("", ...renderSection(comparison, threshold, visibleRows));
  }
  return `${lines.join("\n")}\n`;
}

/**
 * Compares two measurements for every metric.
 *
 * @param {Results} base
 * @param {Results} head
 * @param {number} threshold
 */
export function compareAll(base, head, threshold) {
  return /** @type {Metric[]} */ (["allocated", "instructions", "bytes"]).map(
    metric => compareResults(base, head, threshold, metric),
  );
}

/** @param {string[]} args */
function main(args) {
  const [command, ...rest] = args;
  if (command === "prepare" && rest.length === 1) {
    prepare(path.resolve(rest[0]));
    return 0;
  }
  if (command === "measure" && rest.length >= 2) {
    const [checkout, out, ...options] = rest;
    let instructions = false;
    let playground = false;
    let project;
    for (let i = 0; i < options.length; i++) {
      if (options[i] === "--instructions") instructions = true;
      else if (options[i] === "--playground") playground = true;
      else if (options[i] === "--project" && i + 1 < options.length) {
        project = path.resolve(options[++i]);
      } else throw new Error(`Unknown option ${options[i]}`);
    }
    const results = measure(path.resolve(checkout), {
      instructions,
      project,
      playground,
    });
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
    const comparisons = compareAll(base, head, threshold);
    const summary = renderSummary(comparisons, threshold);
    console.log(summary);
    if (summaryPath) fs.writeFileSync(summaryPath, summary);
    return comparisons.some(c => c.regressions.length > 0) ? 1 : 0;
  }
  console.error(
    "Usage: compilerBenchmarks.js prepare <checkout>\n" +
      "       compilerBenchmarks.js measure <checkout> <out.json> [--instructions]\n" +
      "         [--project <checkout>] [--playground]\n" +
      "       compilerBenchmarks.js compare <base.json> <head.json> [--threshold <percent>]\n" +
      "         [--summary <file.md>]",
  );
  return 2;
}

if (import.meta.filename === path.resolve(process.argv[1] ?? "")) {
  process.exitCode = main(process.argv.slice(2));
}
