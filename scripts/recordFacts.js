#!/usr/bin/env node

// @ts-check

/* Prints the exact forms a commit record abstracts, for one ReScript example,
 * as a given compiler build produces them: the facts an author starts from
 * when writing a change block (.claude/skills/records/SKILL.md). Each view is
 * the compiler's own output; nothing is reformatted except that colour codes
 * are removed.
 *
 *   node scripts/recordFacts.js [--bsc <bsc.exe>] [--jsx] <file.res> [view...]
 *   node scripts/recordFacts.js [--bsc <bsc.exe>] -e '<source>' [view...]
 *
 * Views (default: types lambda js):
 *   parse      the parser's tree (bsc -bs-no-builtin-ppx -only-parse -dparsetree) AST, Syntax
 *   expanded   the tree after the built-in PPX (bsc -only-parse -dparsetree) Transform
 *   transform  the source after the JSX transform (bsc -reprint-source) Transform
 *   format     the formatter's output (bsc -format)                     Format
 *   typed      the typed tree (bsc -dtypedtree)                         Typedtree
 *   types      the inferred interface (rescript-editor-analysis createInterface) Typing, API
 *   lambda     Lambda as translated (bsc -drawlambda)                   Lambda, Matching
 *   passes     Lambda after each pass that changes it (bsc -debug-ir) Lambda
 *   js         the generated ES module (bsc -bs-package-output esmodule) JS
 *   diagnostic what the compiler reports, when it reports anything       Diagnostic
 *   all        every view
 *
 * `--bsc` selects the build: by default the checkout's own
 * packages/@rescript/<platform>/bin/bsc.exe; rescript-editor-analysis.exe is
 * taken from the same directory. `--jsx` passes `-bs-jsx 4`. The example is
 * compiled in a fresh temporary directory, as module `Example` for `-e`. The
 * first line names the build: the compiler's path and the commit its
 * checkout is at, for the record's `Source:` line.
 */

import { spawnSync } from "node:child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { basename, dirname, join, resolve } from "node:path";

const VIEWS = [
  "parse",
  "expanded",
  "transform",
  "format",
  "typed",
  "types",
  "lambda",
  "passes",
  "js",
  "diagnostic",
];

const args = process.argv.slice(2);
/** @param {string} flag */
const option = flag => {
  const at = args.indexOf(flag);
  if (at < 0) return undefined;
  const [, value] = args.splice(at, 2);
  return value;
};
const root = resolve(import.meta.dirname, "..");
const bsc = resolve(
  option("--bsc") ??
    join(
      root,
      "packages",
      "@rescript",
      `${process.platform}-${process.arch}`,
      "bin",
      "bsc.exe",
    ),
);
const source = option("-e");
const jsxAt = args.indexOf("--jsx");
if (jsxAt >= 0) args.splice(jsxAt, 1);
const jsx = jsxAt >= 0 ? ["-bs-jsx", "4"] : [];
const file = source === undefined ? args.shift() : undefined;
const wanted = args.includes("all")
  ? VIEWS
  : args.length
    ? args
    : ["types", "lambda", "js"];
if ((source === undefined && !file) || wanted.some(v => !VIEWS.includes(v))) {
  console.error(
    "usage: recordFacts.js [--bsc <bsc.exe>] [--jsx] <file.res> | -e '<source>' [parse|expanded|transform|format|typed|types|lambda|passes|js|diagnostic|all]...",
  );
  process.exit(1);
}
if (!existsSync(bsc)) {
  console.error(`no compiler at ${bsc}: build one (make) or pass --bsc`);
  process.exit(1);
}

const dir = mkdtempSync(join(tmpdir(), "record-facts-"));
const name = file ? basename(file) : "Example.res";
if (file) copyFileSync(resolve(file), join(dir, name));
else writeFileSync(join(dir, name), `${source}\n`);

/** Terminal colour codes, which the compiler writes into diagnostics. */
const COLOUR = new RegExp(`${String.fromCharCode(27)}\\[[0-9;]*m`, "g");

/**
 * Runs a compiler binary in the example's directory.
 * @param {string} exe
 * @param {string[]} argv
 */
const run = (exe, argv) => {
  const r = spawnSync(exe, argv, { cwd: dir, encoding: "utf8" });
  const plain = (/** @type {string} */ s) => (s ?? "").replace(COLOUR, "");
  return { out: plain(r.stdout), err: plain(r.stderr), ok: r.status === 0 };
};
const base = ["-bs-no-version-header", ...jsx];

/** @type {Record<string, () => string>} */
const show = {
  parse: () =>
    run(bsc, [
      ...base,
      "-bs-no-builtin-ppx",
      "-only-parse",
      "-dparsetree",
      name,
    ]).err,
  expanded: () => run(bsc, [...base, "-only-parse", "-dparsetree", name]).err,
  transform: () => {
    const r = run(bsc, [...jsx, "-reprint-source", name]);
    return r.out || r.err;
  },
  format: () => run(bsc, ["-format", name]).out,
  typed: () => run(bsc, [...base, "-bs-cmi-only", "-dtypedtree", name]).err,
  types: () => {
    const r = run(bsc, [...base, "-bs-cmi-only", name]);
    if (!r.ok) return r.err;
    const analysis = join(dirname(bsc), "rescript-editor-analysis.exe");
    return run(analysis, [
      "createInterface",
      name,
      name.replace(/\.res$/, ".cmi"),
    ]).out;
  },
  lambda: () => {
    const r = run(bsc, [...base, "-drawlambda", name]);
    // -drawlambda prints Lambda on stderr, before the JavaScript on stdout.
    return r.err;
  },
  passes: () => {
    const r = run(bsc, [...base, "-debug-ir", name]);
    if (!r.ok) return r.err;
    const ir = join(dir, name.replace(/\.res$/, ".debug-ir"));
    // A pass is printed when it changes the term; the passes that leave it
    // unchanged are listed by name.
    let previous = "";
    return readdirSync(ir)
      .filter(f => f.endsWith(".lam"))
      .sort()
      .map(f => {
        const term = readFileSync(join(ir, f), "utf8").trimEnd();
        const pass = f.replace(/\.lam$/, "");
        const same = term === previous;
        previous = term;
        return same ? `-- ${pass}: unchanged` : `-- ${pass}\n${term}`;
      })
      .join("\n");
  },
  js: () => {
    mkdirSync(join(dir, "out"), { recursive: true });
    const r = run(bsc, [
      ...base,
      "-bs-project-root",
      dir,
      "-bs-package-name",
      "example",
      "-bs-package-output",
      "esmodule:out:.mjs",
      name,
    ]);
    const out = join(dir, "out", name.replace(/\.res$/, ".mjs"));
    return r.ok && existsSync(out) ? readFileSync(out, "utf8") : r.err;
  },
  diagnostic: () => {
    const r = run(bsc, [...base, name]);
    return r.err.trim() ? r.err : "(no diagnostic)";
  },
};

/** The commit the build's checkout is at: packages/@rescript/<platform>/bin. */
const checkout = resolve(dirname(bsc), "..", "..", "..", "..");
const commit = spawnSync(
  "git",
  ["-C", checkout, "rev-parse", "--short=9", "HEAD"],
  {
    encoding: "utf8",
  },
);
console.log(
  `# ${bsc}${commit.status === 0 ? ` (checkout at ${commit.stdout.trim()})` : ""}\n`,
);
for (const view of wanted)
  console.log(`== ${view}\n${show[view]().trimEnd()}\n`);
