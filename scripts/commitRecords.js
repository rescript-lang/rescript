#!/usr/bin/env node

// @ts-check

/* Renders commit records as a review page. A record is a commit message (or a
 * note on refs/notes/records, which takes precedence) in the format of
 * .claude/skills/records/SKILL.md: a subject, prose sections, code sections
 * such as "Before (js):" and git trailers. The changelog comes from the
 * changes/<description>.<category>.md fragments the commits add, not from the
 * record.
 *
 *   node scripts/commitRecords.js --out review.html <rev>...
 *   node scripts/commitRecords.js --out review.html <base>..<branch>...
 *   --pr <number> and --title <text> head the page with the pull request.
 *   --fragment omits the document skeleton.
 */

import { execFileSync } from "node:child_process";
import { writeFileSync } from "node:fs";
import { parseFragment } from "./changelog.js";

const REPO_URL = "https://github.com/rescript-lang/rescript";

/**
 * @param {string[]} args
 * @param {string} [input]
 */
const git = (args, input) =>
  execFileSync("git", args, {
    input,
    encoding: "utf8",
    maxBuffer: 64 << 20,
    stdio: ["pipe", "pipe", "ignore"],
  });

/** @param {string} s */
const esc = s =>
  s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");

/** @param {string} s */
/** Short shas of the cards on the page, for linking mentions. */
const pageShas = new Set();

/** @param {string} s */
const inline = s =>
  esc(s)
    .replace(/`([^`]+)`/g, "<code>$1</code>")
    .replace(
      /https:\/\/github\.com\/rescript-lang\/rescript\/(pull|issues)\/(\d+)/g,
      `<a href="${REPO_URL}/$1/$2">#$2</a>`,
    )
    .replace(/\b([0-9a-f]{9})\b(\^?)/g, (m, sha, caret) =>
      pageShas.has(sha) && !caret
        ? `<a href="#c-${sha}"><code>${sha}</code></a>`
        : m,
    )
    .replace(
      /(^|[\s(])#(\d{3,5})\b/g,
      `$1<a href="${REPO_URL}/pull/$2">#$2</a>`,
    )
    .replace(
      /(\d[\d.,]*\s*(?:ms|s|%|KB|MB)?)\s*-&gt;\s*(\d[\d.,]*\s*(?:ms|s|%|KB|MB)?)/g,
      '<span class="delta"><span class="from">$1</span> → <span class="to">$2</span></span>',
    );

// ---------------------------------------------------------------------------
// Syntax highlighting

const RES_KEYWORDS = new Set(
  "let rec and switch if else type module open include mutable async await for in to downto while external exception try catch when of as private constraint assert lazy dict list true false".split(
    " ",
  ),
);
const JS_KEYWORDS = new Set(
  "let const var function return if else for while do new import export from as default this true false null undefined typeof instanceof class async await throw try catch finally of in switch case break continue void delete yield".split(
    " ",
  ),
);

/** @type {Record<string, RegExp>} */
const TOKENS = {
  rescript:
    /(\/\/[^\n]*|\/\*[\s\S]*?\*\/)|("(?:\\.|[^"\\\n])*"|`(?:\\.|[^`\\])*`)|(@{1,2}[A-Za-z_][\w.]*|%{1,2}[A-Za-z_][\w.]*)|(#[A-Za-z_]\w*)|(\b0x[0-9a-fA-F_]+n?\b|\b\d[\d_]*(?:\.\d+)?(?:[eE][+-]?\d+)?n?\b)|([A-Za-z_][\w']*)|(=>|->|\+\+|:=|\.\.\.|\|>|[=<>!|&+\-*/:?]+)/g,
  js: /(\/\/[^\n]*|\/\*[\s\S]*?\*\/)|("(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*'|`(?:\\.|[^`\\])*`)|((?!x)x)|((?!x)x)|(\b0x[0-9a-fA-F]+n?\b|\b\d+(?:\.\d+)?(?:[eE][+-]?\d+)?n?\b)|([A-Za-z_$][\w$]*)|(=>|===|!==|\*\*|[=<>!|&+\-*/:?%^]+)/g,
};

/**
 * @param {string} code
 * @param {string} lang
 */
function highlight(code, lang) {
  const re = TOKENS[lang];
  if (!re) return esc(code);
  const keywords = lang === "js" ? JS_KEYWORDS : RES_KEYWORDS;
  let out = "";
  let last = 0;
  re.lastIndex = 0;
  for (let m = re.exec(code); m; m = re.exec(code)) {
    out += esc(code.slice(last, m.index));
    last = m.index + m[0].length;
    const [text, comment, str, attr, poly, num, ident, op] = m;
    const cls = comment
      ? "c"
      : str
        ? "s"
        : attr
          ? "a"
          : poly
            ? "p"
            : num
              ? "n"
              : ident
                ? keywords.has(ident)
                  ? "k"
                  : /^[A-Z]/.test(ident)
                    ? "t"
                    : ""
                : op
                  ? "o"
                  : "";
    // A span never crosses a line break: lines are split after highlighting.
    out += cls
      ? text
          .split("\n")
          .map(part => `<span class="${cls}">${esc(part)}</span>`)
          .join("\n")
      : esc(text);
  }
  return out + esc(code.slice(last));
}

/**
 * Lines of a code block, each highlighted, marked when the paired block of a
 * Before/After pair lacks it.
 * @param {string[]} lines
 * @param {string} lang
 * @param {Set<string> | undefined} other
 * @param {string} mark
 */
function codeLines(lines, lang, other, mark) {
  const text = lines.join("\n").trimEnd();
  const shown = highlight(text, lang).split("\n");
  return text
    .split("\n")
    .map((line, i) => {
      const changed = other && line.trim() && !other.has(line.trim());
      return `<span class="ln${changed ? ` ${mark}` : ""}">${shown[i] || " "}</span>`;
    })
    .join("");
}

// ---------------------------------------------------------------------------
// Records

/**
 * @typedef {{ name: string, lang: string, text: string[], block: string[] }} Section
 * @typedef {{ status: string, path: string, added: number, removed: number }} FileChange
 * @typedef {{ path: string, category: string, text: string, error: string }} Fragment
 * @typedef {{
 *   sha: string, author: string, date: string, subject: string,
 *   sections: Section[], trailers: Map<string, string>, message: string,
 *   coauthors: string[], files: FileChange[], fragments: Fragment[],
 *   patch: string, noted: boolean
 * }} CommitRecord
 */

/** @param {string} text */
function parseTrailers(text) {
  /** @type {Map<string, string>} */
  const trailers = new Map();
  for (const line of git(["interpret-trailers", "--parse"], text).split("\n")) {
    const i = line.indexOf(":");
    if (i <= 0) continue;
    const key = line.slice(0, i).trim();
    const value = line.slice(i + 1).trim();
    trailers.set(
      key,
      trailers.has(key) ? `${trailers.get(key)}\n${value}` : value,
    );
  }
  return trailers;
}

/** @param {Section[]} sections */
function pushProse(sections) {
  /** @type {Section} */
  const s = { name: "", lang: "", text: [], block: [] };
  sections.push(s);
  return s;
}

/** @param {string} path */
const isFragment = path =>
  /^changes\/[^/]+\.md$/.test(path) && path !== "changes/README.md";

/**
 * The changelog fragments a commit adds or changes, as they stand in its tree.
 * A fragment that fails validation (such as one with the placeholder PR link)
 * keeps the category from its name and records the error.
 * @param {string} rev
 * @param {FileChange[]} files
 * @returns {Fragment[]}
 */
function fragmentsOf(rev, files) {
  return files
    .filter(f => f.status !== "D" && isFragment(f.path))
    .map(f => {
      const name = f.path.slice("changes/".length);
      const content = git(["show", `${rev}:${f.path}`]);
      try {
        const { category, text } = parseFragment(name, content);
        return { path: f.path, category, text, error: "" };
      } catch (e) {
        return {
          path: f.path,
          category: /\.([a-z]+)\.md$/.exec(name)?.[1] ?? "?",
          text: content.trim(),
          error: e instanceof Error ? e.message : String(e),
        };
      }
    });
}

/**
 * The bullets of a fragment, without their "- " and with continuation lines
 * joined.
 * @param {string} text
 */
const bullets = text =>
  text
    .split(/\n(?=- )/)
    .map(b => b.replace(/^-\s+/, "").replace(/\n\s+/g, " "))
    .filter(Boolean);

/** @param {string} rev */
function readRecord(rev) {
  let text = "";
  try {
    text = git(["notes", "--ref=records", "show", rev]);
  } catch {
    // no note
  }
  const message = git(["show", "-s", "--format=%B", rev]);
  const noted = text.trim() !== "";
  if (!noted) text = message;
  const lines = text.trimEnd().split("\n");
  let end = lines.length;
  while (
    end > 1 &&
    (/^[A-Za-z-]+: /.test(lines[end - 1]) || /^ \S/.test(lines[end - 1]))
  )
    end--;
  /** @type {Section[]} */
  const sections = [];
  /** @type {Section | undefined} */
  let current;
  for (const line of lines.slice(1, end)) {
    const head =
      /^([A-Z][A-Za-z]*(?: [A-Za-z]+){0,2})(?: \(([a-z]+)\))?:\s*(.*)$/.exec(
        line,
      );
    if (head) {
      current = {
        name: head[1],
        lang: head[2] ?? "",
        text: head[3] ? [head[3]] : [],
        block: [],
      };
      sections.push(current);
    } else if (line.startsWith("    ")) {
      if (!current) current = pushProse(sections);
      current.block.push(line.slice(4));
    } else if (line.trim() === "" && current?.block.length) {
      current.block.push("");
    } else if (line.trim()) {
      if (!current || current.block.length) current = pushProse(sections);
      current.text.push(line.trim());
    } else if (current?.text.length) current = undefined;
  }
  /** @type {Map<string, FileChange>} */
  const files = new Map();
  for (const line of git([
    "diff-tree",
    "--no-commit-id",
    "-r",
    "-M",
    "--name-status",
    rev,
  ]).split("\n")) {
    const [status, ...paths] = line.split("\t");
    if (!status) continue;
    const path = paths[paths.length - 1];
    files.set(path, { status: status[0], path, added: 0, removed: 0 });
  }
  for (const line of git([
    "diff-tree",
    "--no-commit-id",
    "-r",
    "-M",
    "--numstat",
    rev,
  ]).split("\n")) {
    const [a, r, ...rest] = line.split("\t");
    if (!rest.length) continue;
    const path = rest
      .join("\t")
      .replace(/\{[^{}]* => ([^{}]*)\}/, "$1")
      .replace(/^.* => /, "")
      .replace(/\/\//g, "/");
    const f = files.get(path);
    if (f) {
      f.added = Number(a) || 0;
      f.removed = Number(r) || 0;
    }
  }
  /** @type {CommitRecord} */
  const record = {
    sha: git(["rev-parse", "--short=9", rev]).trim(),
    author: git(["show", "-s", "--format=%an", rev]).trim(),
    date: git([
      "show",
      "-s",
      "--format=%ad",
      "--date=format:%b %d, %Y",
      rev,
    ]).trim(),
    subject: lines[0],
    sections,
    trailers: parseTrailers(text),
    message,
    noted,
    coauthors: [
      ...new Set(
        message
          .split("\n")
          .filter(l => /^Co-Authored-By:/i.test(l))
          .map(l =>
            l.replace(/^Co-Authored-By:\s*/i, "").replace(/\s*<[^>]*>/, ""),
          ),
      ),
    ],
    files: [...files.values()],
    fragments: fragmentsOf(rev, [...files.values()]),
    patch: git([
      "show",
      "--format=",
      "-U0",
      rev,
      "--",
      "compiler/ext/config.ml",
    ]),
  };
  return record;
}

/**
 * @param {CommitRecord} r
 * @param {string} key
 */
const words = (r, key) =>
  (r.trailers.get(key) ?? "").split(/\s+/).filter(Boolean);

// ---------------------------------------------------------------------------
// What the diff says

/** @type {[RegExp, string][]} */
const AREAS = [
  [/^compiler\/syntax\//, "syntax"],
  [/^compiler\/core\/lam_/, "lambda"],
  [/^compiler\/core\//, "codegen"],
  [/^compiler\/ml\//, "typechecker"],
  [/^compiler\/frontend\//, "frontend"],
  [/^compiler\/gentype\//, "gentype"],
  [/^compiler\//, "compiler"],
  [/^analysis\/reanalyze\//, "reanalyze"],
  [/^analysis\//, "analysis"],
  [/^rewatch\/tests\//, "tests"],
  [/^rewatch\//, "rewatch"],
  [/^packages\/@rescript\/runtime\//, "runtime"],
  [/^packages\/dev-playground\//, "playground"],
  [/^tools\//, "tools"],
  [/^tests\//, "tests"],
  [/^scripts\//, "scripts"],
  [/^\.github\/|^Makefile$/, "ci"],
  [/^changes\/|^CHANGELOG\.md$/, "changelog"],
  [/\.md$|^docs\//, "docs"],
];

/** @param {string} path */
const areaOf = path => AREAS.find(([re]) => re.test(path))?.[1] ?? "other";

/** @param {string} path */
const isGeneratedJs = path =>
  /\.(mjs|cjs)$|\.res\.js$/.test(path) &&
  /^(tests\/|packages\/@rescript\/runtime\/lib\/|rewatch\/testrepo\/)/.test(
    path,
  );

/**
 * Checks of the record against its own diff. Each is [ok, text].
 * @param {CommitRecord} r
 * @returns {[boolean, string][]}
 */
function checks(r) {
  /** @type {[boolean, string][]} */
  const out = [];
  const has = (/** @type {RegExp} */ re) => r.files.some(f => re.test(f.path));
  for (const f of r.fragments)
    out.push(
      f.error
        ? [false, `${f.path}: ${f.error.replace(/^[^:]*: /, "")}`]
        : [true, `Changelog fragment ${f.path}`],
    );
  if (has(/^CHANGELOG\.md$/))
    out.push([
      false,
      "Edits CHANGELOG.md; a changes/ fragment carries the entry outside release PRs",
    ]);

  const testsValue = r.trailers.get("Tests");
  const testFiles = r.files.filter(f =>
    /^tests\/|^rewatch\/tests\/|\.test\.js$/.test(f.path),
  );
  if (testsValue === undefined) out.push([false, "No Tests trailer"]);
  else if (/^none\b/.test(testsValue))
    out.push(
      testFiles.length
        ? [
            false,
            `Tests: none, but the diff changes ${testFiles.length} test file${testFiles.length === 1 ? "" : "s"}`,
          ]
        : [true, "No test changes, as recorded"],
    );

  // Output of existing programs: modified generated JS. A new test's generated
  // JS is output of a new program, counted separately.
  const changedJs = r.files.filter(
    f => isGeneratedJs(f.path) && f.status !== "A",
  );
  const newJs = r.files.filter(f => isGeneratedJs(f.path) && f.status === "A");
  const jsFiles = (/** @type {number} */ n) =>
    `${n} committed generated JS file${n === 1 ? "" : "s"}`;
  const newNote = newJs.length ? `; ${newJs.length} new` : "";
  const output = r.trailers.get("Output");
  if (output !== undefined) {
    if (/^unchanged/.test(output) && changedJs.length)
      out.push([
        false,
        `Output: unchanged, but ${jsFiles(changedJs.length)} change${changedJs.length === 1 ? "s" : ""}${newNote}`,
      ]);
    else if (/^unchanged/.test(output))
      out.push([true, `No committed generated JS changes${newNote}`]);
    else
      out.push([
        true,
        `${jsFiles(changedJs.length)} change${changedJs.length === 1 ? "s" : ""}${newNote}`,
      ]);
  }

  const bumped = [
    ...r.patch.matchAll(/^\+(?:let|and) (\w+)_magic_number = "([^"]+)"/gm),
  ].map(m => `${m[1]} ${m[2]}`);
  const formats = r.trailers.get("Formats");
  if (bumped.length)
    out.push([
      formats !== undefined && formats !== "none",
      `Magic numbers bumped: ${bumped.join(", ")}`,
    ]);
  else if (has(/^compiler\/ml\/(parsetree|typedtree|types|lambda)\.mli?$/))
    out.push([
      formats !== undefined && formats !== "none",
      "Marshaled AST, typed tree or Lambda type changes with no magic number bump",
    ]);
  if (has(/^compiler\/ml\/parsetree0\.ml$/))
    out.push([false, "Changes the frozen compiler/ml/parsetree0.ml"]);
  if (has(/^compiler\/ml\/ast_mapper_(from|to)0\.ml$/))
    out.push([
      has(
        /^tests\/syntax_tests\/data\/ast-mapping\/|^tests\/ounit_tests\/ounit_ast_mapper0_tests\.ml$/,
      ),
      "v0 PPX bridge changes; ast-mapping fixture or mapper unit test in the diff",
    ]);
  return out;
}

// ---------------------------------------------------------------------------
// Cards

/** @type {Record<string, [string, string]>} */
const CATEGORIES = {
  breaking: ["💥", "Breaking change"],
  feature: ["🚀", "New feature"],
  fix: ["🐛", "Bug fix"],
  docs: ["📝", "Documentation"],
  polish: ["💅", "Polish"],
  internal: ["🏠", "Internal"],
  none: ["", "No changelog"],
};

/** @param {string} category */
const changelogPill = category => {
  const [emoji, label] = CATEGORIES[category] ?? ["", category];
  return `<span class="pill cat-${esc(category)}">${emoji ? `${emoji} ` : ""}${esc(label)}</span>`;
};

/** @param {string} value */
function tests(value) {
  /** @type {Map<string, string[]>} */
  const groups = new Map();
  for (const item of value.split(/\s+(?=[+~-]\S)/)) {
    const kind =
      item[0] === "+"
        ? "add"
        : item[0] === "~"
          ? "upd"
          : item[0] === "-"
            ? "del"
            : "";
    const body = kind ? item.slice(1) : item;
    const suite = /^tests\/syntax_tests\//.test(body)
      ? "syntax"
      : /^tests\/tests\//.test(body)
        ? "integration"
        : /^tests\/ounit_tests\//.test(body)
          ? "ounit"
          : /^tests\/build_tests\/super_errors/.test(body)
            ? "super_errors"
            : /^tests\/build_tests\//.test(body)
              ? "build"
              : /^tests\/analysis_tests\//.test(body)
                ? "analysis"
                : /^tests\/gentype_tests\//.test(body)
                  ? "gentype"
                  : /^rewatch\/tests\//.test(body)
                    ? "rewatch integration"
                    : /^rewatch\/src\//.test(body)
                      ? "cargo test"
                      : "other";
    // The suite's root directory heads the group; items show the rest.
    const root =
      /^(tests\/(syntax_tests\/data|tests\/src|ounit_tests|build_tests)\/|rewatch\/(tests|src)\/)/.exec(
        body,
      )?.[0] ?? "";
    const label = `${suite}${root ? ` <code>${esc(root)}</code>` : ""}`;
    /** @type {Record<string, string>} */
    const MARKS = { add: "+", upd: "~", del: "−" };
    const mark = MARKS[kind] ?? "";
    groups.set(label, [
      ...(groups.get(label) ?? []),
      `<span class="test ${kind}" title="${esc(body)}">${mark ? `<b>${mark}</b> ` : ""}${esc(body.slice(root.length))}</span>`,
    ]);
  }
  return `<div class="tests">${[...groups]
    .map(
      ([label, items]) =>
        `<div><span class="suite">${label}</span>${items.join("")}</div>`,
    )
    .join("")}</div>`;
}

/**
 * A state trailer: its leading word as a chip, the rest as prose.
 * @param {string} v
 */
const stateChip = v => {
  const [, word, rest] = /^([^\s,]+),?\s*(.*)$/s.exec(v) ?? ["", v, ""];
  const cls = /^(unchanged|none)$/.test(word) ? "same" : "diff";
  return `<div class="row"><span class="state ${cls}">${esc(word)}</span>${rest ? `<span>${inline(rest)}</span>` : ""}</div>`;
};

/**
 * Splits on "; " outside parentheses: the items of a Measured trailer.
 * @param {string} v
 */
function items(v) {
  const out = [];
  let depth = 0;
  let start = 0;
  for (let i = 0; i < v.length; i++) {
    if (v[i] === "(") depth++;
    else if (v[i] === ")") depth = Math.max(0, depth - 1);
    else if (v[i] === ";" && depth === 0) {
      out.push(v.slice(start, i).trim());
      start = i + 1;
    }
  }
  out.push(v.slice(start).trim());
  return out.filter(Boolean);
}

const FACTS = [
  "Output",
  "Formatting",
  "Formats",
  "PPX",
  "Editor",
  "Tests",
  "Measured",
];

/** @param {Section[]} sections */
function renderSections(sections) {
  const out = [];
  for (let i = 0; i < sections.length; i++) {
    const s = sections[i];
    if (["Breaking", "Review"].includes(s.name)) continue;
    const next = sections[i + 1];
    if (
      s.name === "Before" &&
      next?.name === "After" &&
      next.lang === s.lang &&
      s.block.length &&
      next.block.length
    ) {
      const before = new Set(s.block.map(l => l.trim()));
      const after = new Set(next.block.map(l => l.trim()));
      out.push(
        `<div class="pair">${[
          [s, after, "del", "Before"],
          [next, before, "add", "After"],
        ]
          .map(
            ([sec, other, mark, label]) =>
              `<figure><figcaption><b>${label}</b> <span class="lang">${esc(langName(/** @type {Section} */ (sec).lang))}</span>${
                /** @type {Section} */ (sec).text.length
                  ? ` · ${inline(/** @type {Section} */ (sec).text.join(" "))}`
                  : ""
              }</figcaption><pre class="code">${codeLines(
                /** @type {Section} */ (sec).block,
                /** @type {Section} */ (sec).lang,
                /** @type {Set<string>} */ (other),
                /** @type {string} */ (mark),
              )}</pre></figure>`,
          )
          .join("")}</div>`,
      );
      i++;
      continue;
    }
    if (s.name === "Source" && !s.block.length) {
      out.push(
        `<p class="source"><b>Source</b> ${inline(s.text.join(" "))}</p>`,
      );
      continue;
    }
    if (s.name)
      out.push(
        `<h4>${esc(s.name)}${s.lang ? ` <span class="lang">${esc(langName(s.lang))}</span>` : ""}</h4>`,
      );
    if (s.text.length) out.push(`<p>${inline(s.text.join(" "))}</p>`);
    if (s.block.length) {
      const lang = s.lang || (s.name === "Example" ? "rescript" : "");
      const body =
        lang === "text"
          ? esc(s.block.join("\n").trimEnd())
          : codeLines(s.block, lang, undefined, "");
      out.push(
        `<pre class="code${lang === "text" ? " term" : ""}">${body}</pre>`,
      );
    }
  }
  return out.join("\n");
}

/** @param {string} lang */
const langName = lang =>
  ({
    rescript: "ReScript",
    js: "JavaScript",
    ts: "TypeScript",
    sh: "shell",
    rust: "Rust",
    ocaml: "OCaml",
    text: "output",
  })[lang] ?? lang;

/** @param {CommitRecord} r */
function card(r) {
  const categories = [...new Set(r.fragments.map(f => f.category))];
  const pr = (
    r.trailers.get("PR") ??
    /\(#(\d+)\)\s*$/.exec(r.subject)?.[0] ??
    ""
  ).replace(/\D/g, "");
  const subject = r.subject.replace(/\s*\(#\d+\)\s*$/, "");
  const out = [
    `<article class="card${categories.includes("breaking") ? " breaking" : ""}" id="c-${r.sha}">`,
  ];
  out.push(
    `<header><div class="meta"><code>${r.sha}</code> · ${esc(r.author)} · ${esc(r.date)}${
      pr ? ` · <a href="${REPO_URL}/pull/${pr}">#${pr} discussion ↗</a>` : ""
    }${r.noted ? ' · <span title="record from refs/notes/records">note</span>' : ""}</div>` +
      `<h3>${inline(subject)}</h3>` +
      `<div class="chips">${categories.map(changelogPill).join("")}${words(
        r,
        "Area",
      )
        .map(a => `<span class="chip">${esc(a)}</span>`)
        .join("")}</div></header>`,
  );
  const breaking = r.sections.find(s => s.name === "Breaking");
  if (breaking)
    out.push(
      `<div class="banner"><strong>Breaking</strong> ${inline([...breaking.text, ...breaking.block].join(" "))}</div>`,
    );
  out.push(renderSections(r.sections));
  for (const f of r.fragments)
    out.push(
      `<div class="entry">${changelogPill(f.category)}<span>${bullets(f.text).map(inline).join("<br>")}</span></div>`,
    );
  const reviews = r.sections.filter(s => s.name === "Review");
  if (reviews.length)
    out.push(
      `<h4>Review</h4><ul class="review">${reviews
        .map(s => `<li>${inline([...s.text, ...s.block].join(" "))}</li>`)
        .join("")}</ul>`,
    );
  const STATES = ["Output", "Formatting", "Formats", "PPX", "Editor"];
  const same = STATES.filter(k =>
    /^(unchanged|none)$/.test(r.trailers.get(k) ?? ""),
  );
  const facts = FACTS.flatMap(k => {
    const v = r.trailers.get(k);
    if (v === undefined || same.includes(k)) return [];
    const one = v.replace(/\n/g, " ");
    const listed = items(one);
    const shown =
      k === "Tests"
        ? /^none\b/.test(one)
          ? stateChip(one)
          : tests(one)
        : STATES.includes(k)
          ? stateChip(one)
          : k === "Measured" && listed.length > 1
            ? `<ul class="run">${listed.map(l => `<li>${inline(l)}</li>`).join("")}</ul>`
            : v
                .split("\n")
                .map(line => `<span>${inline(line)}</span>`)
                .join("");
    return [`<dt>${k}</dt><dd>${shown}</dd>`];
  });
  if (same.length)
    facts.unshift(
      `<dt>No effect</dt><dd class="row">${same.map(k => `<span class="state same">${k}: ${esc(r.trailers.get(k) ?? "")}</span>`).join("")}</dd>`,
    );
  if (facts.length) out.push(`<dl class="facts">${facts.join("")}</dl>`);
  out.push(
    `<ul class="checks">${checks(r)
      .map(
        ([ok, text]) => `<li class="${ok ? "ok" : "warn"}">${esc(text)}</li>`,
      )
      .join("")}</ul>`,
  );
  /** @type {Map<string, {n: number, a: number, d: number}>} */
  const byArea = new Map();
  for (const f of r.files) {
    const a = areaOf(f.path);
    const e = byArea.get(a) ?? { n: 0, a: 0, d: 0 };
    byArea.set(a, { n: e.n + 1, a: e.a + f.added, d: e.d + f.removed });
  }
  const added = r.files.reduce((n, f) => n + f.added, 0);
  const removed = r.files.reduce((n, f) => n + f.removed, 0);
  out.push(
    `<details><summary>${r.files.length} files · <span class="plus">+${added}</span> <span class="minus">−${removed}</span> · ${[
      ...byArea,
    ]
      .map(([a, e]) => `${esc(a)} ${e.n}`)
      .join(" · ")}</summary><table class="files">${r.files
      .map(
        f =>
          `<tr><td class="st st-${f.status}">${f.status}</td><td><code>${esc(f.path)}</code></td><td class="num plus">+${f.added}</td><td class="num minus">−${f.removed}</td></tr>`,
      )
      .join("")}</table></details>`,
  );
  if (r.coauthors.length)
    out.push(`<div class="meta">with ${esc(r.coauthors.join(" · "))}</div>`);
  out.push("</article>");
  return out.join("\n");
}

// ---------------------------------------------------------------------------
// Ranges

/**
 * @typedef {{ name: string, range?: string, records: CommitRecord[] }} Group
 */

/** @param {string} arg */
function group(arg) {
  if (!arg.includes("..")) return { name: arg, records: [readRecord(arg)] };
  const revs = git(["rev-list", "--reverse", "--no-merges", arg])
    .split("\n")
    .filter(Boolean);
  return {
    name: arg.split("..")[1],
    range: arg,
    records: revs.map(readRecord),
  };
}

/**
 * The fragments a range leaves in changes/: the last version of each one its
 * commits add or change, minus those a later commit deletes.
 * @param {Group} g
 */
function rangeFragments(g) {
  /** @type {Map<string, Fragment>} */
  const byPath = new Map();
  for (const r of g.records) {
    for (const f of r.files) if (f.status === "D") byPath.delete(f.path);
    for (const f of r.fragments) byPath.set(f.path, f);
  }
  return [...byPath.values()];
}

/** @param {Group[]} groups */
function summary(groups) {
  const ranges = groups.filter(g => g.range);
  if (!ranges.length) return "";
  const rows = ranges.map(g => {
    const cats = new Map();
    for (const f of rangeFragments(g))
      cats.set(f.category, (cats.get(f.category) ?? 0) + 1);
    if (!cats.size) cats.set("none", 1);
    const areas = [...new Set(g.records.flatMap(r => words(r, "Area")))];
    const js = g.records.filter(r =>
      /^changed/.test(r.trailers.get("Output") ?? ""),
    ).length;
    const formats = [
      ...new Set(
        g.records
          .map(r => r.trailers.get("Formats") ?? "none")
          .filter(v => v !== "none"),
      ),
    ];
    const warnings = g.records.reduce(
      (n, r) => n + checks(r).filter(([ok]) => !ok).length,
      0,
    );
    return (
      `<tr><td class="nowrap"><a href="#g-${esc(g.name)}"><code>${esc(g.range ?? g.name)}</code></a></td><td class="num">${g.records.length}</td>` +
      `<td>${[...cats].map(([c, n]) => `${changelogPill(c)}${n > 1 ? ` ×${n}` : ""}`).join(" ")}</td>` +
      `<td>${areas.map(a => `<span class="chip">${esc(a)}</span>`).join("")}</td>` +
      `<td class="num">${js}</td><td>${formats.map(inline).join("<br>") || '<span class="muted">none</span>'}</td>` +
      `<td class="num">${warnings ? `<span class="warn-n">${warnings}</span>` : "0"}</td></tr>`
    );
  });
  /** @type {Map<string, string[]>} */
  const owners = new Map();
  for (const g of ranges)
    for (const r of g.records)
      for (const f of r.files)
        owners.set(f.path, [
          ...(owners.get(f.path) ?? []),
          ranges.length > 1 ? g.name : r.sha,
        ]);
  const shared = [...owners]
    .map(([f, os]) => /** @type {[string, string[]]} */ ([f, [...new Set(os)]]))
    .filter(([, os]) => os.length > 1)
    .sort();
  /** @type {Map<string, string[]>} */
  const draft = new Map();
  for (const g of ranges)
    for (const f of rangeFragments(g))
      draft.set(f.category, [
        ...(draft.get(f.category) ?? []),
        ...bullets(f.text),
      ]);
  const order = Object.keys(CATEGORIES);
  return (
    `<section><h2>Summary</h2><div class="table-wrap"><table><thead><tr><th>Range</th><th>Commits</th><th>Changelog</th><th>Areas</th><th>JS output changes</th><th>Formats</th><th>Failed checks</th></tr></thead><tbody>${rows.join("")}</tbody></table></div>` +
    (draft.size
      ? `<h4>Changelog draft</h4><div class="draft">${[...draft]
          .sort((a, b) => order.indexOf(a[0]) - order.indexOf(b[0]))
          .map(
            ([c, es]) =>
              `<div>${changelogPill(c)}<ul>${es.map(e => `<li>${inline(e)}</li>`).join("")}</ul></div>`,
          )
          .join("")}</div>`
      : "") +
    (shared.length
      ? `<h4>Files changed by more than one ${ranges.length > 1 ? "range" : "commit"}</h4><div class="table-wrap"><table><tbody>${shared
          .map(
            ([f, os]) =>
              `<tr><td><code>${esc(f)}</code></td><td>${os.map(o => (ranges.length > 1 ? esc(o) : `<a href="#c-${o}"><code>${o}</code></a>`)).join(", ")}</td></tr>`,
          )
          .join("")}</tbody></table></div>`
      : "") +
    `</section>`
  );
}

// ---------------------------------------------------------------------------
// Page

const LIGHT = `--bg:#f7f6f4; --surface:#ffffff; --ink:#1f1d1c; --muted:#6b6560; --rule:#e2ded9; --accent:#d43a3f; --accent-soft:#fbe9e9;
  --code-bg:#f4f2ef; --code-ink:#2a2725; --k:#a3238e; --s:#1f7a3a; --n:#b35c00; --c:#8a8580; --t:#1d5fa8; --a:#8a5a00; --o:#6b6560; --p:#7a3db8;
  --add:#e3f4e6; --del:#fbe5e3; --ok:#23784a; --ok-bg:#e2f3e8; --warn:#9a5b00; --warn-bg:#fdf0d9;
  --same:#5a6470; --same-bg:#eceff2; --diff:#1d5fa8; --diff-bg:#e3edf9; --plus:#23784a; --minus:#b3261e;`;
const DARK = `--bg:#151413; --surface:#1e1c1b; --ink:#ece8e4; --muted:#a39d97; --rule:#36322f; --accent:#ff6b6f; --accent-soft:#3a1d1e;
  --code-bg:#252220; --code-ink:#e6e1dc; --k:#e48ad6; --s:#8fd6a0; --n:#f2b36b; --c:#8a847e; --t:#8ab8f0; --a:#e0bc6a; --o:#b5aea7; --p:#c6a2f0;
  --add:#1c3324; --del:#3a1f1d; --ok:#8fd6a0; --ok-bg:#1c3324; --warn:#f2c36b; --warn-bg:#3a2d16;
  --same:#b0b8c2; --same-bg:#2a2e33; --diff:#8ab8f0; --diff-bg:#1d2b3d; --plus:#8fd6a0; --minus:#f19a93; color-scheme:dark;`;

const CSS = `
:root { ${LIGHT}
  --display:"Fraunces",Georgia,serif; --body:"Inter","Helvetica Neue",Arial,sans-serif; --mono:"JetBrains Mono",Menlo,Consolas,monospace; }
@media (prefers-color-scheme: dark) { :root:not([data-theme="light"]) { ${DARK} } }
:root[data-theme="dark"] { ${DARK} }
* { box-sizing:border-box } body { margin:0; background:var(--bg); color:var(--ink); font:15px/1.6 var(--body) }
main { max-width:1120px; margin:0 auto; padding:32px 16px 64px; display:flex; flex-direction:column; gap:22px }
h1 { font:600 30px/1.2 var(--display); margin:0; text-wrap:balance } h1.brand { font-size:34px } h1 span, .kicker span { color:var(--accent) } h1 a { text-decoration:none }
.kicker { font:600 13px/1 var(--display); letter-spacing:.04em; margin:0 0 8px; color:var(--muted) }
h2 { font:600 22px/1.2 var(--display); margin:16px 0 8px; padding-top:14px; border-top:2px solid var(--ink) }
h3 { font:600 20px/1.3 var(--display); margin:4px 0 8px; text-wrap:balance }
h4 { font:600 11px/1 var(--body); letter-spacing:.08em; text-transform:uppercase; color:var(--muted); margin:14px 0 6px }
p { margin:2px 0; max-width:75ch } .lede, .muted { color:var(--muted) }
a { color:var(--accent) } :focus-visible { outline:2px solid var(--accent); outline-offset:2px }
code { font:12.5px/1.5 var(--mono); background:var(--code-bg); padding:1px 4px; border-radius:3px; overflow-wrap:anywhere }
.card { background:var(--surface); border:1px solid var(--rule); border-radius:10px; padding:20px 22px; display:flex; flex-direction:column; gap:6px; min-width:0 }
.card.breaking { border-top:4px solid var(--accent) }
.meta { font:12px/1.5 var(--mono); color:var(--muted) } .meta code { background:none; padding:0 }
.chips { display:flex; flex-wrap:wrap; gap:6px; align-items:center }
.chip { font:500 12px/1 var(--body); border:1px solid var(--rule); border-radius:99px; padding:5px 9px; color:var(--muted) }
.pill { font:600 12px/1 var(--body); padding:6px 10px; border-radius:99px; white-space:nowrap; background:var(--same-bg); color:var(--same) }
.pill.cat-breaking { background:var(--accent); color:#fff } .pill.cat-fix { background:var(--warn-bg); color:var(--warn) }
.pill.cat-feature { background:var(--ok-bg); color:var(--ok) } .pill.cat-polish { background:var(--diff-bg); color:var(--diff) }
.banner { background:var(--accent-soft); border-left:4px solid var(--accent); padding:10px 14px; border-radius:6px; margin:4px 0 }
.banner strong { color:var(--accent); margin-right:6px; text-transform:uppercase; font-size:12px; letter-spacing:.06em }
pre { margin:4px 0; padding:12px 0; border-radius:8px; overflow-x:auto; background:var(--code-bg); color:var(--code-ink); font:12.5px/1.6 var(--mono) }
pre .ln { display:block; padding:0 14px; white-space:pre; min-height:1.6em } pre .ln.add { background:var(--add) } pre .ln.del { background:var(--del) }
pre.term { padding:12px 14px; white-space:pre }
.k { color:var(--k) } .s { color:var(--s) } .n { color:var(--n) } .c { color:var(--c); font-style:italic } .t { color:var(--t) } .a { color:var(--a) } .o { color:var(--o) } .p { color:var(--p) }
.pair { display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); grid-template-rows:auto auto; column-gap:10px; margin:8px 0 }
figure { margin:0; min-width:0; display:grid; grid-row:span 2; grid-template-rows:subgrid; row-gap:0 } figure pre { margin:0 } figcaption { font:12px/1.4 var(--body); color:var(--muted); margin-bottom:4px } figcaption b { font-weight:600; font-size:11px; letter-spacing:.06em; text-transform:uppercase }
.lang { font:500 10px/1 var(--mono); letter-spacing:0; text-transform:none; border:1px solid var(--rule); border-radius:4px; padding:2px 5px; color:var(--muted) }
.entry { display:flex; gap:10px; align-items:baseline; background:var(--code-bg); padding:10px 12px; border-radius:8px; margin-top:8px; font-size:14px; flex-wrap:wrap }
.entry span:last-child { flex:1 1 300px; min-width:0 }
ul.review { margin:0; padding-left:18px } ul.review li { margin:2px 0 }
dl.facts { display:grid; grid-template-columns:max-content minmax(0,1fr); gap:8px 16px; margin:12px 0 0; padding-top:12px; border-top:1px solid var(--rule); font-size:13.5px }
dt { font:600 11px/2 var(--body); letter-spacing:.06em; text-transform:uppercase; color:var(--muted) }
dd { margin:0; min-width:0; display:flex; flex-direction:column; gap:3px } dd.row, .row { display:flex; flex-direction:row; flex-wrap:wrap; gap:6px; align-items:baseline }
.source { font-size:12.5px; color:var(--muted) } .source b { font-weight:600; font-size:11px; letter-spacing:.06em; text-transform:uppercase; margin-right:4px }
.state { align-self:flex-start; padding:3px 8px; border-radius:5px } .state.same { background:var(--same-bg); color:var(--same) } .state.diff { background:var(--diff-bg); color:var(--diff) }
.delta { font-variant-numeric:tabular-nums; white-space:nowrap } .delta .from { color:var(--muted); text-decoration:line-through } .delta .to { font-weight:600; color:var(--ok) }
.tests { display:flex; flex-direction:column; gap:4px } .tests > div { display:flex; flex-wrap:wrap; gap:4px; align-items:baseline }
.suite { font:600 11px/1 var(--body); color:var(--muted); min-width:9em }
.test { font:12px/1.3 var(--mono); padding:3px 7px; border-radius:4px; background:var(--code-bg); overflow-wrap:anywhere }
.test.add { background:var(--ok-bg); color:var(--ok) } .test.del { background:var(--del); color:var(--minus) } .test b { font-weight:600 }
.suite code { font-size:11px; font-weight:400; background:none; padding:0 } .run { margin:0; padding-left:18px } .run li { margin:1px 0 }
ul.checks { list-style:none; margin:10px 0 0; padding:0; display:flex; flex-wrap:wrap; gap:6px; font-size:12.5px }
ul.checks li { padding:3px 9px; border-radius:5px } ul.checks li.ok { background:var(--ok-bg); color:var(--ok) } ul.checks li.ok::before { content:"✓ " }
ul.checks li.warn { background:var(--warn-bg); color:var(--warn) } ul.checks li.warn::before { content:"! " }
details { margin-top:8px; font-size:13px; color:var(--muted) } summary { cursor:pointer }
table { border-collapse:collapse; width:100%; font-size:13px } th, td { text-align:left; vertical-align:top; padding:7px 10px; border-bottom:1px solid var(--rule) }
th { font:600 11px/1.2 var(--body); letter-spacing:.06em; text-transform:uppercase; color:var(--muted); white-space:nowrap }
td.num { font-variant-numeric:tabular-nums; text-align:right; white-space:nowrap } .plus { color:var(--plus) } .minus { color:var(--minus) }
table.files td { padding:3px 8px } td.st { font:600 11px var(--mono) } .st-A { color:var(--plus) } .st-D { color:var(--minus) }
.table-wrap { overflow-x:auto; border:1px solid var(--rule); border-radius:10px; background:var(--surface) }
.draft { display:flex; flex-direction:column; gap:10px; background:var(--surface); border:1px solid var(--rule); border-radius:10px; padding:14px 16px }
td.nowrap code { white-space:nowrap } .draft ul { margin:6px 0 0; padding-left:18px } .warn-n { color:var(--warn); font-weight:600 }
pre, code, .test { font-variant-ligatures:none; font-feature-settings:"calt" 0, "liga" 0 }
@media (max-width:720px) { .pair { grid-template-columns:minmax(0,1fr); grid-template-rows:none; row-gap:10px } figure { grid-row:auto; grid-template-rows:auto auto } dl.facts { grid-template-columns:minmax(0,1fr) } dt { line-height:1.2; margin-top:6px } .card { padding:16px } }`;

const args = process.argv.slice(2);
/** @type {Record<string, string>} */
const options = {};
const targets = [];
let fragment = false;
for (let i = 0; i < args.length; i++) {
  if (["--out", "--pr", "--title"].includes(args[i]) && i + 1 < args.length)
    options[args[i].slice(2)] = args[++i];
  else if (args[i] === "--fragment") fragment = true;
  else targets.push(args[i]);
}
const outFile = options.out;
if (!outFile) {
  console.error(
    "usage: commitRecords.js --out <file.html> [--pr <number>] [--title <text>] [--fragment] <rev | base..branch>...",
  );
  process.exit(1);
}
const pr = (options.pr ?? "").replace(/\D/g, "");
const title = options.title ?? "";
const groups = targets.map(group);
for (const g of groups) for (const r of g.records) pageShas.add(r.sha);
const commits = groups.reduce((n, g) => n + g.records.length, 0);
const commitCount = `${commits} commit${commits === 1 ? "" : "s"}, rendered from ${commits === 1 ? "its record" : "their records"}.`;
const heading =
  pr || title
    ? `<p class="kicker">Semantic <span>Diff</span></p><h1>${pr ? `<a href="${REPO_URL}/pull/${pr}">#${pr}</a> ` : ""}${inline(title)}</h1>`
    : `<h1 class="brand">Semantic <span>Diff</span></h1>`;
const body = `<main><header>${heading}<p class="lede">${commitCount}</p></header>
${summary(groups)}
${groups
  .map(
    g =>
      (g.range
        ? `<h2 id="g-${esc(g.name)}"><code>${esc(g.range)}</code></h2>`
        : "") + g.records.map(card).join("\n"),
  )
  .join("\n")}
</main>`;
const pageTitle =
  pr || title
    ? `${pr ? `#${pr} ` : ""}${title} · Semantic Diff`
    : "Semantic Diff";
const head = `<title>${esc(pageTitle)}</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,600&family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400;500&display=swap">
<style>${CSS}</style>`;
writeFileSync(
  outFile,
  fragment
    ? `${head}\n${body}\n`
    : `<!doctype html>\n<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">\n${head}</head><body>${body}</body></html>\n`,
);
console.log(outFile);
