// @ts-check

/* Commit records, parsed from a commit message and drawn as HTML. Pure: no
 * file system and no git, so the review page (commitRecords.js) and any later
 * reader of commit messages, such as a pull request userscript, draw the same
 * records the same way. The format is the `records` skill's
 * (.claude/skills/records/SKILL.md): a subject, a `Why:` section, change
 * blocks such as `Typing modified (rule name):`, examples, and git trailers.
 * The changelog is the commit's changes/<name>.<category>.md fragments, which
 * the caller reads and validates.
 */

const REPO_URL = "https://github.com/rescript-lang/rescript";

/** @param {string} s */
export const esc = s =>
  s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");

// ---------------------------------------------------------------------------
// The kinds of object a record changes, and how each is drawn

/**
 * How a block's content is drawn.
 * @typedef {"math" | "tree" | "matrix" | "mapping" | "rescript" | "js" | "ts" | "ocaml" | "lambda" | "text" | "effect" | "prose"} Drawing
 */

/**
 * The kinds of object a change block names (the skill states what each is,
 * which layer owns it and how to extract it). Each kind has one drawing and
 * every drawing has a function in DRAW: a kind added here without a drawing,
 * or a drawing without a function, does not type-check.
 * @typedef {"Why" | "Syntax" | "Format" | "AST" | "Transform" | "Typing" | "Typedtree" | "Diagnostic" | "Matching" | "Lambda" | "IR" | "Representation" | "JS" | "API" | "TypeScript" | "Editor" | "Build" | "Semantics" | "Notation" | "Example" | "Counterexample" | "Effect" | "Code" | "Docs" | "CI"} Kind
 */

/** @type {{ [K in Kind]: Drawing }} */
export const KINDS = {
  Why: "prose",
  Syntax: "math",
  Format: "rescript",
  AST: "tree",
  Transform: "rescript",
  Typing: "math",
  Typedtree: "tree",
  Diagnostic: "text",
  Matching: "matrix",
  Lambda: "lambda",
  IR: "ocaml",
  Representation: "mapping",
  JS: "js",
  API: "rescript",
  TypeScript: "ts",
  Editor: "text",
  Build: "prose",
  Semantics: "math",
  Notation: "math",
  Example: "rescript",
  Counterexample: "rescript",
  Effect: "effect",
  Code: "prose",
  Docs: "prose",
  CI: "prose",
};

/** @param {string} name @returns {name is Kind} */
const isKind = name => Object.hasOwn(KINDS, name);

/** Kinds whose parenthesis names a program or a version, not an object. */
const UNNAMED = new Set(["Why", "Example", "Counterexample", "Effect"]);

/**
 * A parenthesis naming a language draws an unknown section as that code:
 * `Command (sh):`, `Output (text):`.
 * @type {Record<string, Drawing>}
 */
const LANGS = {
  rescript: "rescript",
  js: "js",
  ts: "ts",
  ocaml: "ocaml",
  lambda: "lambda",
  text: "text",
  sh: "text",
  json: "js",
};

// ---------------------------------------------------------------------------
// Records

/**
 * @typedef {{ name: string, text: string[], block: string[] }} Section
 * @typedef {{ status: string, path: string, added: number, removed: number }} FileChange
 * @typedef {{ path: string, category: string, text: string, error: string }} Fragment
 * A changelog fragment as it stands in the commit's tree; `error` is the
 * reason `yarn changelog:check` rejects it, or "".
 * @typedef {{
 *   sha: string, author: string, date: string, message: string,
 *   noted: boolean, files: FileChange[], fragments: Fragment[], patch: string
 * }} CommitInfo
 * `fragments` are the changelog fragments the commit adds or changes;
 * `patch` is the diff of compiler/ext/config.ml, where the magic numbers are.
 * @typedef {CommitInfo & {
 *   subject: string, sections: Section[], trailers: Map<string, string>,
 *   coauthors: string[]
 * }} CommitRecord
 * @typedef {{ name: string, range?: string, records: CommitRecord[] }} Group
 */

/**
 * The trailers of a message: its last paragraph, when every line of it is
 * `Key: value` or a continuation line starting with a space, as
 * `git interpret-trailers --parse` reads it. A repeated key's values join
 * with a line break.
 * @param {string} text
 */
export function parseTrailers(text) {
  /** @type {Map<string, string>} */
  const trailers = new Map();
  const paragraphs = text.trimEnd().split(/\n\s*\n/);
  if (paragraphs.length < 2) return trailers;
  const last = paragraphs[paragraphs.length - 1].split("\n");
  const isTrailer = (/** @type {string} */ l) => /^[A-Za-z0-9-]+: /.test(l);
  if (!isTrailer(last[0]) || !last.every(l => isTrailer(l) || /^\s/.test(l)))
    return trailers;
  /** @type {[string, string][]} */
  const entries = [];
  for (const l of last) {
    if (/^\s/.test(l)) entries[entries.length - 1][1] += ` ${l.trim()}`;
    else {
      const i = l.indexOf(":");
      entries.push([l.slice(0, i).trim(), l.slice(i + 1).trim()]);
    }
  }
  for (const [key, value] of entries) {
    const old = trailers.get(key);
    trailers.set(key, old === undefined ? value : `${old}\n${value}`);
  }
  return trailers;
}

/**
 * A commit's record from its text (its note, else its message), with what
 * git says of the commit.
 * @param {string} text
 * @param {CommitInfo} commit
 * @returns {CommitRecord}
 */
export function parseRecord(text, commit) {
  const trailers = parseTrailers(text);
  const lines = text.trimEnd().split("\n");
  let end = lines.length;
  if (trailers.size) while (end > 1 && lines[end - 1].trim()) end--;
  /** @type {Section[]} */
  const sections = [];
  /** @type {Section | undefined} */
  let current;
  const prose = () => {
    /** @type {Section} */
    const s = { name: "", text: [], block: [] };
    sections.push(s);
    return s;
  };
  for (const line of lines.slice(1, end)) {
    const head =
      /^([A-Z][A-Za-z]*(?: [a-z][A-Za-z]*){0,3}(?: \((?:[^()]|\([^()]*\))*\))?):\s*(.*)$/.exec(
        line,
      );
    if (head) {
      current = { name: head[1], text: head[2] ? [head[2]] : [], block: [] };
      sections.push(current);
    } else if (/^ {2,}\S/.test(line)) {
      current ??= prose();
      current.block.push(line);
    } else if (line.trim()) {
      if (!current || current.block.length) current = prose();
      current.text.push(line.trim());
    } else if (current?.block.length) current.block.push("");
    else if (current && !current.name) current = undefined;
  }
  const coauthors = commit.message
    .split("\n")
    .filter(l => /^Co-Authored-By:/i.test(l))
    .map(l => l.replace(/^Co-Authored-By:\s*/i, "").replace(/\s*<[^>]*>/, ""));
  return {
    ...commit,
    subject: lines[0],
    sections,
    trailers,
    coauthors: [...new Set(coauthors)],
  };
}

// ---------------------------------------------------------------------------
// Code, highlighted by token

/**
 * A language's tokens: a regular expression whose named groups are the
 * token classes, and its keywords among the identifiers.
 * @typedef {{ token: RegExp, keywords: Set<string> }} Lang
 */

/** @param {string} s */
const words = s => new Set(s.split(" "));

/** @type {{ [L in "rescript" | "js" | "ts" | "ocaml" | "lambda"]: Lang }} */
const LANG = {
  rescript: {
    token:
      /(?<c>\/\/[^\n]*|\/\*[\s\S]*?\*\/)|(?<s>"(?:\\.|[^"\\\n])*"|`(?:\\.|[^`\\])*`|'(?:\\.|[^'\\\n])')|(?<a>@{1,2}[A-Za-z_][\w.]*|%{1,2}[A-Za-z_][\w.]*)|(?<p>#[A-Za-z_]\w*)|(?<n>\b\d[\d_]*(?:\.\d+)?(?:[eE][+-]?\d+)?n?\b|\b0[xob][0-9a-fA-F_]+\b)|(?<v>'[a-z]\w*)|(?<i>[A-Za-z_][\w']*)/g,
    keywords: words(
      "let rec and switch if else type module open include mutable async await for in to downto while external exception try catch when as private constraint assert dict list true false break continue of",
    ),
  },
  js: {
    token:
      /(?<c>\/\/[^\n]*|\/\*[\s\S]*?\*\/)|(?<s>"(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*'|`(?:\\.|[^`\\])*`)|(?<n>\b\d+(?:\.\d+)?(?:[eE][+-]?\d+)?n?\b|\b0x[0-9a-fA-F]+\b)|(?<i>[A-Za-z_$][\w$]*)/g,
    keywords: words(
      "let const var function return if else for while do new import export from as default this true false null undefined typeof instanceof class async await throw try catch finally of in switch case break continue void delete yield",
    ),
  },
  ts: {
    token:
      /(?<c>\/\/[^\n]*|\/\*[\s\S]*?\*\/)|(?<s>"(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*'|`(?:\\.|[^`\\])*`)|(?<n>\b\d+(?:\.\d+)?\b)|(?<i>[A-Za-z_$][\w$]*)/g,
    keywords: words(
      "let const var function return if else import export from as default type interface readonly extends keyof typeof null undefined true false string number boolean bigint unknown never void any",
    ),
  },
  ocaml: {
    token:
      /(?<c>\(\*[\s\S]*?\*\))|(?<s>"(?:\\.|[^"\\\n])*")|(?<v>'[a-z]\w*)|(?<n>\b\d+(?:\.\d+)?\b)|(?<i>[A-Za-z_][\w']*)/g,
    keywords: words(
      "type and of let rec in match with function fun if then else mutable private module struct sig end open include val external exception when as option list",
    ),
  },
  lambda: {
    token:
      /(?<s>"(?:\\.|[^"\\\n])*")|(?<p>#[A-Za-z_][\w.]*)|(?<k>module\/exports)|(?<i>[A-Za-z_][\w$.']*)(?<stamp>\/\d+)?|(?<n>-?\b\d+(?:\.\d+)?[nl]?\b)|(?<paren>[()])/g,
    keywords: words(
      "let letrec function if switch stringswitch staticcatch staticraise catch exit try with while for seq apply makeblock field setfield default case",
    ),
  },
};

/**
 * Code highlighted by token: comments, strings, attributes and extensions,
 * polymorphic variants, numbers, keywords, capitalised names (modules,
 * constructors), type variables; in Lambda, primitives (`#is_not_none`),
 * identifier stamps (`x/1002`), parentheses, and one-letter capitals as
 * metavariables standing for a term.
 * @param {string} code
 * @param {keyof typeof LANG} lang
 */
export function highlight(code, lang) {
  const { token, keywords } = LANG[lang];
  let out = "";
  let last = 0;
  for (const m of code.matchAll(token)) {
    out += esc(code.slice(last, m.index));
    last = (m.index ?? 0) + m[0].length;
    const g = m.groups ?? {};
    const span = (/** @type {string} */ cls, /** @type {string} */ t) =>
      t
        .split("\n")
        .map(part => `<span class="${cls}">${esc(part)}</span>`)
        .join("\n");
    if (g.i !== undefined) {
      const id = g.i;
      out += keywords.has(id)
        ? span("k", id)
        : lang === "lambda" && /^[A-Z]$/.test(id)
          ? `<var>${esc(id)}</var>`
          : /^[A-Z]/.test(id)
            ? span("t", id)
            : esc(id);
      if (g.stamp) out += span("stamp", g.stamp);
    } else {
      const cls = ["c", "s", "a", "p", "n", "v", "k", "paren"].find(
        k => g[k] !== undefined,
      );
      out += cls ? span(cls, m[0]) : esc(m[0]);
    }
  }
  return out + esc(code.slice(last));
}

/**
 * The pairs of line numbers a longest common subsequence of `a` and `b`
 * matches, in order, lines compared by `key`.
 * @param {string[]} a
 * @param {string[]} b
 * @param {(line: string) => string} key
 * @returns {[number, number][]}
 */
function matchLines(a, b, key) {
  const x = a.map(key);
  const y = b.map(key);
  /** @type {number[][]} */
  const t = Array.from({ length: x.length + 1 }, () =>
    new Array(y.length + 1).fill(0),
  );
  for (let i = x.length - 1; i >= 0; i--)
    for (let j = y.length - 1; j >= 0; j--)
      t[i][j] =
        x[i] === y[j]
          ? t[i + 1][j + 1] + 1
          : Math.max(t[i + 1][j], t[i][j + 1]);
  /** @type {[number, number][]} */
  const pairs = [];
  for (let i = 0, j = 0; i < x.length && j < y.length; ) {
    if (x[i] === y[j]) pairs.push([i++, j++]);
    else if (t[i + 1][j] >= t[i][j + 1]) i++;
    else j++;
  }
  return pairs;
}

/**
 * The lines of `a` and of `b` outside a longest common subsequence: the
 * lines a Before/After pair changes. Lines compare without surrounding
 * spaces.
 * @param {string[]} a
 * @param {string[]} b
 * @returns {[Set<number>, Set<number>]}
 */
export function lineDiff(a, b) {
  const pairs = matchLines(a, b, l => l.trim());
  /** @param {string[]} ls @param {Set<number>} keep */
  const changed = (ls, keep) =>
    new Set(ls.flatMap((l, n) => (keep.has(n) || !l.trim() ? [] : [n])));
  return [
    changed(a, new Set(pairs.map(p => p[0]))),
    changed(b, new Set(pairs.map(p => p[1]))),
  ];
}

/**
 * The rows of a Before/After pair drawn side by side: a line common to both
 * versions is one row; between two common lines, the lines only Before has
 * and the lines only After has share rows, the shorter side padded with
 * gaps (`null`). Lines compare without trailing spaces, so a line that moves
 * to another depth is a change.
 * @param {string[]} a
 * @param {string[]} b
 * @returns {{ a: number | null, b: number | null, same: boolean }[]}
 */
export function alignLines(a, b) {
  /** @type {{ a: number | null, b: number | null, same: boolean }[]} */
  const rows = [];
  let i = 0;
  let j = 0;
  const ends = [...matchLines(a, b, l => l.trimEnd()), [a.length, b.length]];
  for (const [x, y] of ends) {
    for (let k = 0; k < Math.max(x - i, y - j); k++)
      rows.push({
        a: i + k < x ? i + k : null,
        b: j + k < y ? j + k : null,
        same: false,
      });
    if (x < a.length) rows.push({ a: x, b: y, same: true });
    i = x + 1;
    j = y + 1;
  }
  return rows;
}

/**
 * @typedef {{ marked?: Set<number>, mark?: "add" | "del" }} Marks
 * The lines of one version of a Before/After pair that the other lacks.
 */

/**
 * One drawn line. A line too long for its column wraps, its continuation
 * indented two columns past `indent`, the column its content starts at.
 * @param {string} html
 * @param {number} indent
 * @param {string} [cls]
 * @param {string} [style]
 */
const line = (html, indent, cls = "", style = "") =>
  `<span class="ln${cls ? ` ${cls}` : ""}" style="${style}--i:${indent}">${html || " "}</span>`;

/** The column a line's content starts at. @param {string} l */
const indentOf = l => /^ */.exec(l)?.[0].length ?? 0;

/**
 * Lines of code, each drawn by `draw`, as HTML, one string a line.
 * @param {string[]} lines
 * @param {(code: string) => string} draw
 */
const codeHtml = (lines, draw) => draw(lines.join("\n")).split("\n");

/**
 * Lines of code, each drawn by `draw`, the marked ones shaded.
 * @param {string[]} lines
 * @param {(code: string) => string} draw
 * @param {Marks} [marks]
 */
function codeLines(lines, draw, marks = {}) {
  return `<pre class="code">${codeHtml(lines, draw)
    .map((l, i) =>
      line(l, indentOf(lines[i] ?? ""), marks.marked?.has(i) ? marks.mark : ""),
    )
    .join("")}</pre>`;
}

/**
 * Code, or a rewrite of code: a line holding `⇝` separates the code before
 * it from the code after it (`lhs ⇝ rhs` on one line, or `⇝` alone on a
 * line); `label: text` lines after the right side are side conditions,
 * drawn as mathematics.
 * @param {string[]} lines
 * @param {keyof typeof LANG} lang
 * @param {Marks} [marks]
 */
function code(lines, lang, marks) {
  const draw = (/** @type {string} */ c) => highlight(c, lang);
  if (!lines.some(l => l.includes("⇝"))) return codeLines(lines, draw, marks);
  return `<div class="rewrites">${groups(lines)
    .map(g => {
      const at = g.findIndex(l => l.includes("⇝"));
      if (at < 0) return codeLines(g, draw);
      const [left, right] = g[at].split("⇝");
      const rest = g.slice(at + 1);
      const cond = rest.findIndex(l => /^\s*[a-z][\w ]{0,20}:\s/.test(l));
      const lhs = dedent([...g.slice(0, at), ...(left.trim() ? [left] : [])]);
      const rhs = dedent([
        ...(right.trim() ? [right.trim()] : []),
        ...(cond < 0 ? rest : rest.slice(0, cond)),
      ]);
      return (
        `<div class="rewrite">${codeLines(lhs, draw)}<span class="to">⇝</span>${codeLines(rhs, draw)}</div>` +
        (cond < 0 ? [] : rest.slice(cond)).map(condition).join("")
      );
    })
    .join("")}</div>`;
}

/** A side condition: `label: formula`, or a formula. @param {string} l */
function condition(l) {
  const m = /^\s*([a-z][\w ]{0,20}):\s+(.*)$/.exec(l);
  return `<div class="cond">${m ? `<span class="label">${esc(m[1])}</span>` : ""}<span class="m">${math(m ? m[2] : l.trim())}</span></div>`;
}

// ---------------------------------------------------------------------------
// Inline text

/** Short shas of the cards on the page, for linking mentions. */
const pageShas = new Set();

/**
 * Prose: `code` as highlighted ReScript, pull request and issue links,
 * mentions of commits on the page.
 * @param {string} s
 */
export const inline = s =>
  s
    .split(/(`[^`]+`)/)
    .map((part, i) =>
      i % 2
        ? `<code>${highlight(part.slice(1, -1), "rescript")}</code>`
        : esc(part)
            .replace(
              /https:\/\/github\.com\/rescript-lang\/rescript\/(pull|issues)\/(\d+)/g,
              `<a href="${REPO_URL}/$1/$2">#$2</a>`,
            )
            .replace(
              /(^|[\s(])#(\d{3,5})\b/g,
              `$1<a href="${REPO_URL}/pull/$2">#$2</a>`,
            )
            .replace(/\b([0-9a-f]{9})\b(\^?)/g, (m, sha, caret) =>
              pageShas.has(sha) && !caret
                ? `<a href="#c-${sha}"><code>${sha}</code></a>`
                : m,
            )
            .replace(
              /(\d[\d.,]*\s*(?:ms|s|%|KB|MB)?)\s*-&gt;\s*(\d[\d.,]*\s*(?:ms|s|%|KB|MB)?)/g,
              '<span class="delta"><span class="from">$1</span> → <span class="to">$2</span></span>',
            ),
    )
    .join("");

// ---------------------------------------------------------------------------
// Mathematics: typing rules, grammar, semantics

/* Mathematics set as type theory sets it: one-letter names (variables,
 * metavariables) italic; a name applied to arguments, `fields(τ)`, an upright
 * operator; a capitalised name an upright constructor; binders and relations
 * coloured; `code` in backticks highlighted ReScript; a run of two or more
 * words, `τ is an @unboxed record`, a side condition in text. `_x`, `_{…}`
 * and `^{…}` are sub- and superscripts. The input is the record's; nothing
 * is added to it. */
const MATH_KEYWORDS = words("if then else let in switch");
const CONNECTIVES = words("or and not");
/** Literal values, drawn as code wherever they appear. */
const LITERALS = words("null undefined true false NaN");
const BINDERS = new Set("∀∃λΛμ");
const RELATIONS = new Set("⊢⊨≤≥⊑⊒∈∉⊆⊂=≠≡:⇝↦≜≔→⟶");
const TEXT_WORD =
  /^(?:a|an|A|An|[A-Z]{2,}|[A-Za-z][A-Za-z'-]*[a-z][A-Za-z'-]*)$/;
/** English words that never name a type, even after a relation. */
const STOP_WORDS = words(
  "the with of for is are to from where when that which its their this each every",
);

/**
 * @typedef {"code" | "str" | "script" | "word" | "num" | "space" | "sym"} TokenKind
 * @typedef {{ kind: TokenKind, text: string }} MathToken
 * @typedef {"text" | "code" | "str" | "lit" | "script" | "kw" | "op" | "var" | "con" | "bind" | "rel" | "plain"} MathRole
 */

/** @param {string} text @returns {MathToken[]} */
function mathTokens(text) {
  const token =
    /(?<code>`[^`]+`)|(?<str>"[^"\n]*")|(?<script>_\{[^{}]*\}|\^\{[^{}]*\}|_[A-Za-z0-9Ͱ-Ͽ]+)|(?<word>@?[A-Za-zͰ-Ͽ][A-Za-z0-9_Ͱ-Ͽ'′-]*⃗?)|(?<num>\d+)|(?<space>\s+)|(?<sym>[\s\S])/g;
  return [...text.matchAll(token)].map(m => {
    const g = m.groups ?? {};
    /** @type {TokenKind[]} */
    const kinds = ["code", "str", "script", "word", "num", "space", "sym"];
    return {
      kind: kinds.find(k => g[k] !== undefined) ?? "sym",
      text: m[0],
    };
  });
}

/**
 * What each token of a formula is: a word of a side condition, code, a
 * quoted string, a sub- or superscript, a keyword, an operator applied to
 * arguments, a one-letter variable, a capitalised constructor, a binder, a
 * relation, or other symbols and spaces.
 * @param {MathToken[]} ts
 * @returns {MathRole[]}
 */
function mathRoles(ts) {
  // Side conditions in words: two or more words, or one lowercase word of
  // three letters or more, not applied to arguments; a run continues across
  // spaces, punctuation, quoted strings and code.
  /** @type {Set<number>} */
  const prose = new Set();
  /** A lone word just after a relation (`: bool`, `= int`) names a type. */
  const afterRelation = (/** @type {number} */ k) => {
    let j = k - 1;
    while (ts[j]?.kind === "space") j--;
    return ts[j] !== undefined && RELATIONS.has(ts[j].text);
  };
  const applied = (/** @type {number} */ k) =>
    ts[k + 1]?.text === "(" ||
    ts[k + 1]?.text === "<" ||
    ts[k + 1]?.kind === "script";
  const textWord = (/** @type {number} */ k) =>
    ts[k]?.kind === "word" &&
    (TEXT_WORD.test(ts[k].text) || ts[k].text.startsWith("@")) &&
    !MATH_KEYWORDS.has(ts[k].text) &&
    !LITERALS.has(ts[k].text) &&
    !applied(k);
  const joins = (/** @type {number} */ k) =>
    ["space", "code", "str"].includes(ts[k].kind) ||
    ",;()".includes(ts[k].text) ||
    (ts[k].kind === "word" && MATH_KEYWORDS.has(ts[k].text));
  for (let i = 0; i < ts.length; i++) {
    if (!textWord(i)) continue;
    const run = [i];
    for (let k = i + 1; k < ts.length && (joins(k) || textWord(k)); k++)
      if (textWord(k)) run.push(k);
    if (
      run.length < 2 &&
      !(
        /^[a-z]{3,}$/.test(ts[i].text) &&
        (!afterRelation(i) || STOP_WORDS.has(ts[i].text))
      )
    )
      continue;
    // A parenthesis or string closing the run belongs to it when the run
    // holds its start; an opening parenthesis just before it too.
    const start = ts[i - 1]?.text === "(" ? i - 1 : i;
    let end = run[run.length - 1];
    while (
      end + 1 < ts.length &&
      (ts[end + 1].kind === "str" ||
        (ts[end + 1].text === ")" &&
          ts.slice(start, end + 1).some(t => t.text === "(")))
    )
      end++;
    for (let k = start; k <= end; k++) prose.add(k);
    i = end;
  }
  return ts.map((t, i) => {
    if (t.kind === "code" || t.kind === "str" || t.kind === "script")
      return t.kind;
    if (prose.has(i)) return "text";
    if (t.kind === "word") {
      const w = t.text;
      if (MATH_KEYWORDS.has(w)) return "kw";
      if (LITERALS.has(w)) return "lit";
      if (CONNECTIVES.has(w)) return "text";
      if (/^[a-z]{3,}$/.test(w) && afterRelation(i) && !STOP_WORDS.has(w))
        return "con";
      if (applied(i) && w.length > 1) return "op";
      if (/^[A-Za-zͰ-Ͽ][0-9'′]*⃗?$/.test(w)) return "var";
      return /^[A-Z]/.test(w) ? "con" : "op";
    }
    if (t.kind === "sym")
      return BINDERS.has(t.text)
        ? "bind"
        : RELATIONS.has(t.text)
          ? "rel"
          : "plain";
    return "plain";
  });
}

/** @type {{ [R in MathRole]: string }} */
const MATH_CLASS = {
  text: "m-text",
  code: "",
  str: "",
  lit: "m-lit",
  script: "",
  kw: "m-kw",
  op: "m-op",
  var: "",
  con: "m-con",
  bind: "m-bind",
  rel: "m-rel",
  plain: "",
};

/** @param {string} text @returns {string} */
function math(text) {
  const ts = mathTokens(text);
  const roles = mathRoles(ts);
  const prose = (/** @type {number} */ i) =>
    roles[i] === "text" ||
    ((roles[i] === "code" || roles[i] === "str") &&
      (roles[i - 1] === "text" || roles[i + 1] === "text"));
  return ts
    .map((t, i) => {
      const e = esc(t.text);
      const r = roles[i];
      let piece = e;
      if (r === "code")
        piece = `<code class="m-code">${highlight(t.text.slice(1, -1), "rescript")}</code>`;
      else if (r === "str")
        piece = `<code class="m-code"><span class="s">${e}</span></code>`;
      else if (r === "script") {
        const tag = t.text.startsWith("^") ? "sup" : "sub";
        piece = `<${tag}>${math(t.text.replace(/^[_^]\{?|\}$/g, ""))}</${tag}>`;
      } else if (r === "var") piece = `<var>${e}</var>`;
      else if (MATH_CLASS[r] && r !== "text")
        piece = `<span class="${MATH_CLASS[r]}">${e}</span>`;
      if (!prose(i)) return piece;
      return `${prose(i - 1) ? "" : '<span class="m-text">'}${piece}${prose(i + 1) ? "" : "</span>"}`;
    })
    .join("");
}

/** A line's parts: runs of three or more spaces separate side-by-side formulas. @param {string} line */
const parts = line =>
  line
    .trim()
    .split(/\s{3,}/)
    .filter(Boolean);

/** Formulas, one row per line, an indented line continuing the one above. @param {string[]} lines */
const formulas = lines =>
  `<div class="formulas">${lines
    .map(
      l =>
        `<div class="row${/^\s{2,}/.test(l) ? " cont" : ""}">${parts(l)
          .map(p => `<span>${math(p)}</span>`)
          .join("")}</div>`,
    )
    .join("")}</div>`;

/** Groups of lines separated by blank lines. @param {string[]} block */
function groups(block) {
  /** @type {string[][]} */
  const out = [[]];
  for (const line of block) {
    if (line.trim()) out[out.length - 1].push(line);
    else if (out[out.length - 1].length) out.push([]);
  }
  return out.filter(g => g.length);
}

/**
 * Mathematics: groups separated by blank lines; a group with a line of `─`
 * is an inference rule (premises above, conclusion below, the rule's name in
 * parentheses after the bar); any other group is a list of formulas.
 * @param {string[]} block
 * @param {Marks} [marks]
 */
function rules(block, marks = {}) {
  const html = groups(block).map(g => {
    const bar = g.findIndex(l => /^\s*─{3,}/.test(l));
    if (bar < 0) {
      return formulas(g);
    }
    const name = /\(([^()]+)\)\s*$/.exec(g[bar])?.[1] ?? "";
    const premises = g.slice(0, bar).flatMap(parts);
    const conclusion = g.slice(bar + 1);
    return (
      `<div class="infer"><div class="frac"><div class="premises">${premises
        .map(p => `<span>${math(p)}</span>`)
        .join(
          "",
        )}</div><div class="bar"></div><div class="conclusion">${conclusion
        .map(l => `<div>${math(l.trim())}</div>`)
        .join("")}</div></div>` +
      (name ? `<span class="rule-name">(${esc(name)})</span>` : "") +
      `</div>`
    );
  });
  const changed = marks.marked?.size ? ` ${marks.mark}` : "";
  return `<div class="rules${changed}">${html.join("")}</div>`;
}

// ---------------------------------------------------------------------------
// Trees, matrices, mappings

/**
 * Whether a line of a tree is drawn: a line holding only `[` or `]`, the
 * list brackets of the compiler's tree printers, is not; the elements
 * inside are indented one level deeper than the node owning the list.
 * @param {string} l
 */
const treeLine = l => l.trim() !== "" && !/^\s*[[\]]\s*$/.test(l);

/**
 * One node of a tree: `label: Node …` shows its label muted; constructors
 * (`Pexp_ternary`, `Some`) are coloured, strings and numbers literals,
 * attributes (`@res.ternary`) marked.
 * @param {string} t
 */
function treeNode(t) {
  const lab = /^([a-z_][\w ]*):\s+(.*)$/.exec(t);
  const drawn = (lab ? lab[2] : t)
    .split(/("(?:\\.|[^"\\])*")/)
    .map((part, i) =>
      i % 2
        ? `<span class="s">${esc(part)}</span>`
        : esc(part)
            .replace(/(^|\s)(@[\w.]+)/g, '$1<span class="a">$2</span>')
            .replace(/\b([A-Z][A-Za-z0-9_]*)\b/g, '<span class="t">$1</span>')
            .replace(/(^|[\s(=])(\d+)\b/g, '$1<span class="n">$2</span>'),
    )
    .join("");
  return (lab ? `<span class="label">${esc(lab[1])}:</span> ` : "") + drawn;
}

/**
 * A tree written as indented lines, two spaces a level, drawn with
 * connectors: for each line drawn, its index, its HTML and the column its
 * node starts at.
 * @param {string[]} lines
 * @returns {{ i: number, html: string, indent: number }[]}
 */
function treeRows(lines) {
  const rows = lines
    .map((l, i) => ({ i, depth: indentOf(l) >> 1, l }))
    .filter(r => treeLine(r.l));
  return rows.map((r, k) => {
    // For each ancestor level, whether a later sibling still follows.
    let prefix = "";
    for (let d = 1; d <= r.depth; d++) {
      let more = false;
      for (let n = k + 1; n < rows.length && rows[n].depth >= d; n++)
        if (rows[n].depth === d) {
          more = true;
          break;
        }
      // No-break spaces keep a node on the line of its connector.
      prefix +=
        d === r.depth
          ? more
            ? "├─\u00a0"
            : "└─\u00a0"
          : more
            ? "│\u00a0\u00a0"
            : "\u00a0\u00a0\u00a0";
    }
    return {
      i: r.i,
      html: `<span class="branch">${prefix}</span>${treeNode(r.l.trim())}`,
      indent: prefix.length,
    };
  });
}

/**
 * A tree, drawn with connectors, the marked lines shaded.
 * @param {string[]} lines
 * @param {Marks} [marks]
 */
function tree(lines, marks = {}) {
  return `<pre class="code tree">${treeRows(lines)
    .map(r => line(r.html, r.indent, marks.marked?.has(r.i) ? marks.mark : ""))
    .join("")}</pre>`;
}

/**
 * A clause matrix of the match compiler: one row a line, columns three or
 * more spaces apart, each cell a ReScript pattern; `label: text` lines are
 * notes below it.
 * @param {string[]} lines
 * @param {Marks} [marks]
 */
function matrix(lines, marks = {}) {
  const rows = lines.map((l, i) => ({ i, l })).filter(r => r.l.trim());
  const cells = rows.filter(r => !/^\s*[a-z][\w ]{0,20}:\s/.test(r.l));
  const notes = rows.filter(r => /^\s*[a-z][\w ]{0,20}:\s/.test(r.l));
  const width = Math.max(1, ...cells.map(r => parts(r.l).length));
  return (
    `<div class="matrix" style="grid-template-columns:repeat(${width},max-content)">${cells
      .map(r => {
        const mark = marks.marked?.has(r.i) ? ` ${marks.mark}` : "";
        const ps = parts(r.l);
        return ps
          .concat(Array(width - ps.length).fill(""))
          .map(
            p =>
              `<code class="cell${mark}">${highlight(p, "rescript") || " "}</code>`,
          )
          .join("");
      })
      .join("")}</div>` + notes.map(r => condition(r.l)).join("")
  );
}

/**
 * How ReScript values are represented in JavaScript: `ReScript ↦ JavaScript`
 * lines, each side highlighted; `label: text` lines are conditions.
 * @param {string[]} lines
 * @param {Marks} [marks]
 */
function mapping(lines, marks = {}) {
  return `<div class="mapping">${lines
    .map((l, i) => {
      if (!l.trim()) return "";
      const mark = marks.marked?.has(i) ? ` ${marks.mark}` : "";
      const at = l.indexOf("↦");
      if (at < 0) return condition(l);
      return `<div class="map${mark}"><code>${highlight(l.slice(0, at).trim(), "rescript")}</code><span class="to">↦</span><code>${highlight(l.slice(at + 1).trim(), "js")}</code></div>`;
    })
    .join("")}</div>`;
}

/**
 * `<program or case>   <old> → <new>`, three or more spaces apart.
 * @param {string[]} lines
 */
const effect = lines =>
  `<ul class="effect">${lines
    .filter(l => l.trim())
    .map(l => {
      const [what, ...rest] = parts(l);
      return `<li><code>${highlight(what, "rescript")}</code>${rest
        .map(
          x =>
            ` ${inline(x).replace(/ → /g, ' <span class="arrow">→</span> ')}`,
        )
        .join("")}</li>`;
    })
    .join("")}</ul>`;

/**
 * Prose: paragraphs separated by blank lines; lines indented further than
 * the paragraphs are a ReScript code block.
 * @param {string[]} lines
 */
function prose(lines) {
  /** @type {{ code: boolean, lines: string[] }[]} */
  const chunks = [];
  for (const l of lines) {
    const code = /^\s/.test(l);
    const last = chunks[chunks.length - 1];
    if (!l.trim()) {
      if (last?.code) last.lines.push("");
      else chunks.push({ code: false, lines: [] });
    } else if (last && last.code === code && (code || last.lines.length))
      last.lines.push(l);
    else chunks.push({ code, lines: [l] });
  }
  return `<div class="prose">${chunks
    .filter(c => c.lines.some(l => l.trim()))
    .map(c =>
      c.code
        ? codeLines(dedent(c.lines), t => highlight(t, "rescript"))
        : `<p>${inline(c.lines.join(" ").replace(/\s+/g, " ").trim())}</p>`,
    )
    .join("")}</div>`;
}

/**
 * Each drawing's function, given the lines of one version and, in a
 * Before/After pair, the lines the other version lacks.
 * @type {{ [D in Drawing]: (lines: string[], marks?: Marks) => string }}
 */
const DRAW = {
  math: (lines, marks) => rules(lines, marks),
  tree: (lines, marks) => tree(lines, marks),
  matrix: (lines, marks) => matrix(lines, marks),
  mapping: (lines, marks) => mapping(lines, marks),
  rescript: (lines, marks) => code(lines, "rescript", marks),
  js: (lines, marks) => code(lines, "js", marks),
  ts: (lines, marks) => code(lines, "ts", marks),
  ocaml: (lines, marks) => code(lines, "ocaml", marks),
  lambda: (lines, marks) => code(lines, "lambda", marks),
  text: (lines, marks) =>
    `<div class="text">${codeLines(lines, esc, marks)}</div>`,
  effect: lines => effect(lines),
  prose: lines => prose(lines),
};

/**
 * Lines without their common indentation and trailing blank lines.
 * @param {string[]} lines
 */
function dedent(lines) {
  const indents = lines
    .filter(l => l.trim())
    .map(l => /^ */.exec(l)?.[0].length ?? 0);
  const cut = indents.length ? Math.min(...indents) : 0;
  const out = lines.map(l => l.slice(cut));
  while (out.length && !out[out.length - 1].trim()) out.pop();
  return out;
}

/**
 * One section: a change block (`<Kind> <added|modified|removed> (<object>):`),
 * a kind with a version or program in parentheses (`JS (before):`), or prose.
 * A block holding `Before:` and `After:` lines (or `Before (lang):` and
 * `After (lang):`, drawn as that language) draws its two versions side by
 * side, the lines each lacks from the other shaded; a block with only one of
 * them, an object added or removed, draws that version across the block.
 * @param {Section} s
 */
export function section(s) {
  const m =
    /^([A-Z][A-Za-z]*)(?: (added|modified|removed))?(?: \(((?:[^()]|\([^()]*\))*)\))?$/.exec(
      s.name,
    );
  const kind = m?.[1] ?? s.name;
  const action = m?.[2] ?? "";
  const target = m?.[3] ?? "";
  let drawing = isKind(kind) ? KINDS[kind] : (LANGS[target] ?? "prose");
  const lines = dedent(s.block);
  const named = target && !UNNAMED.has(kind) && !LANGS[target];
  const head = s.name
    ? `<h4 class="change">${action ? `<span class="act ${action}">${action}</span>` : ""}<span class="kind">${esc(kind)}</span>${
        named || (target && UNNAMED.has(kind))
          ? ` <span class="target">${inline(target)}</span>`
          : ""
      }</h4>`
    : "";
  const text = s.text.length ? `<p>${inline(s.text.join(" "))}</p>` : "";
  const wrap = (/** @type {string} */ body) =>
    `<section class="block k-${esc(kind.toLowerCase())}">${head}${text}${body}</section>`;
  if (!lines.length) return wrap("");
  const label = /^(Before|After)(?: \(([a-z]+)\))?:$/;
  if (!lines.some(l => label.test(l.trim()))) return wrap(DRAW[drawing](lines));
  /** @type {{ lead: string[], before: string[], after: string[] }} */
  const v = { lead: [], before: [], after: [] };
  /** @type {"lead" | "before" | "after"} */
  let into = "lead";
  for (const l of lines) {
    const version = label.exec(l.trim());
    if (version) {
      into = version[1] === "Before" ? "before" : "after";
      if (version[2] && LANGS[version[2]]) drawing = LANGS[version[2]];
    } else v[into].push(l);
  }
  const before = dedent(v.before);
  const after = dedent(v.after);
  const lead = v.lead.length ? prose(dedent(v.lead)) : "";
  if (!before.length || !after.length)
    return wrap(lead + DRAW[drawing](before.length ? before : after));
  return wrap(lead + versions(drawing, before, after));
}

/** Drawings whose lines are drawn one a row, so two versions align by line. */
const LINED = new Set([
  "tree",
  "rescript",
  "js",
  "ts",
  "ocaml",
  "lambda",
  "text",
]);

/**
 * Two versions of an object side by side, each in the drawing of its kind,
 * the lines each lacks from the other shaded. A caption may follow the
 * version's label. A drawing of lines (code, a tree, text) is drawn as rows:
 * a line common to both versions faces itself, and the lines between are
 * padded so that the next common line faces itself again.
 * @param {Drawing} drawing
 * @param {string[]} b
 * @param {string[]} a
 * @param {[string, string]} [captions]
 */
function versions(drawing, b, a, captions = ["", ""]) {
  const head = (
    /** @type {string} */ label,
    /** @type {string} */ caption,
    tag = "figcaption",
  ) =>
    `<${tag} class="vlabel ${label === "Before" ? "del" : "add"}">${label}${caption ? ` <span class="cap">${caption}</span>` : ""}</${tag}>`;
  const rewrite = [...b, ...a].some(l => l.includes("⇝"));
  if (LINED.has(drawing) && !rewrite)
    return `<div class="pair">${head("Before", captions[0], "div")}${head("After", captions[1], "div")}${pairRows(drawing, b, a)}</div>`;
  const [del, add] = lineDiff(b, a);
  const version = (
    /** @type {string} */ label,
    /** @type {string} */ caption,
    /** @type {string[]} */ ls,
    /** @type {Marks} */ marks,
  ) => `<figure>${head(label, caption)}${DRAW[drawing](ls, marks)}</figure>`;
  return `<div class="versions">${version("Before", captions[0], b, { marked: del, mark: "del" })}${version("After", captions[1], a, { marked: add, mark: "add" })}</div>`;
}

/**
 * The rows of a pair of versions in a drawing of lines. Each line is a cell
 * placed at its row and column (`--r`, `--c`). Within a run of changed
 * lines the Before cells come first, then the After ones, so that a narrow
 * screen, which stacks the cells in one column and hides the After copy of
 * each common line and the gaps, reads the pair as a unified diff.
 * @param {Drawing} drawing
 * @param {string[]} b
 * @param {string[]} a
 */
function pairRows(drawing, b, a) {
  const isTree = drawing === "tree";
  const before = isTree ? b.filter(treeLine) : b;
  const after = isTree ? a.filter(treeLine) : a;
  /** @param {string[]} ls @returns {{ html: string, indent: number }[]} */
  const cells = ls => {
    if (isTree) return treeRows(ls);
    const html =
      drawing === "text"
        ? ls.map(esc)
        : codeHtml(ls, c =>
            highlight(c, /** @type {keyof typeof LANG} */ (drawing)),
          );
    return html.map((h, i) => ({ html: h, indent: indentOf(ls[i]) }));
  };
  const left = cells(before);
  const right = cells(after);
  const rows = alignLines(before, after);
  const at = (
    /** @type {number} */ r,
    /** @type {1 | 2} */ c,
    /** @type {{ html: string, indent: number } | undefined} */ cell,
    /** @type {string} */ cls,
  ) =>
    cell
      ? line(cell.html, cell.indent, cls, `--r:${r};--c:${c};`)
      : line("", 0, "gap", `--r:${r};--c:${c};`);
  const out = [
    '<span class="pad" style="--r:1;--c:1"></span><span class="pad dup" style="--r:1;--c:2"></span>',
  ];
  for (let n = 0; n < rows.length; ) {
    const row = rows[n];
    if (row.same) {
      out.push(at(n + 2, 1, left[row.a ?? 0], ""));
      out.push(at(n + 2, 2, right[row.b ?? 0], "dup"));
      n++;
      continue;
    }
    let end = n;
    while (end < rows.length && !rows[end].same) end++;
    const run = rows.slice(n, end);
    /** @param {number | null} k @param {string[]} ls @param {string} cls */
    const mark = (k, ls, cls) => (k !== null && ls[k].trim() ? cls : "");
    for (const [k, x] of run.entries())
      out.push(
        at(
          n + k + 2,
          1,
          x.a === null ? undefined : left[x.a],
          mark(x.a, before, "del"),
        ),
      );
    for (const [k, x] of run.entries())
      out.push(
        at(
          n + k + 2,
          2,
          x.b === null ? undefined : right[x.b],
          mark(x.b, after, "add"),
        ),
      );
    n = end;
  }
  const last = rows.length + 2;
  out.push(
    `<span class="pad" style="--r:${last};--c:1"></span><span class="pad dup" style="--r:${last};--c:2"></span>`,
  );
  return `<div class="rows code${isTree ? " tree" : ""}${drawing === "text" ? " text" : ""}">${out.join("")}</div>`;
}

/**
 * The language a section's parenthesis names: `Before (js)` is JavaScript.
 * @param {string} name
 */
const langOf = name => /\(([a-z]+)\)$/.exec(name)?.[1] ?? "";

/**
 * A top-level `Before (lang):` section followed by `After (lang):` in the
 * same language: one pair, side by side.
 * @param {Section} before
 * @param {Section} after
 */
function sectionPair(before, after) {
  const lang = langOf(before.name);
  const caption = (/** @type {Section} */ s) =>
    `<span class="lang">${esc(lang)}</span>${s.text.length ? ` · ${inline(s.text.join(" "))}` : ""}`;
  return `<section class="block">${versions(
    LANGS[lang] ?? "text",
    dedent(before.block),
    dedent(after.block),
    [caption(before), caption(after)],
  )}</section>`;
}

/**
 * The sections of a record in order, a top-level Before/After pair of one
 * language drawn as one pair.
 * @param {Section[]} sections
 */
function drawSections(sections) {
  const out = [];
  for (let i = 0; i < sections.length; i++) {
    const s = sections[i];
    const next = sections[i + 1];
    if (
      /^Before \([a-z]+\)$/.test(s.name) &&
      next?.name === `After (${langOf(s.name)})` &&
      s.block.length &&
      next.block.length
    ) {
      out.push(sectionPair(s, next));
      i++;
    } else out.push(section(s));
  }
  return out.join("\n");
}

// ---------------------------------------------------------------------------
// What the diff says beside the record

/** @param {string} path */
const isGeneratedJs = path =>
  /^(tests|rewatch\/testrepo)\/.*\.(mjs|cjs|res\.js)$/.test(path) ||
  /^packages\/@rescript\/[^/]+\/lib\/.*\.(mjs|js)$/.test(path);

/** @param {string} path */
const isTestFile = path => /^tests\/|^rewatch\/tests\/|\.test\.js$/.test(path);

/**
 * A snapshot a test generates: committed generated JavaScript, a file in an
 * `expected/` directory, an `.expected` file, a rewatch snapshot. A `Tests`
 * trailer may leave it out.
 * @param {string} path
 */
const isSnapshot = path =>
  isGeneratedJs(path) ||
  /\/expected\/|\.expected$|^rewatch\/tests\/snapshots\//.test(path);

/**
 * The items of a `Tests` trailer that lists tests: `+added ~changed -removed`
 * paths, each with its mark.
 * @param {string} value
 * @returns {{ mark: "+" | "~" | "-" | "", path: string }[]}
 */
const testItems = value =>
  value
    .split(/\s+(?=[+~-]\S)/)
    .map(item => item.trim())
    .filter(Boolean)
    .map(item =>
      /^[+~-]/.test(item)
        ? {
            mark: /** @type {"+" | "~" | "-"} */ (item[0]),
            path: item.slice(1),
          }
        : { mark: "", path: item },
    );

/**
 * Checks of the record against its own diff, each [ok, text]: the changelog
 * fragments it adds or changes, the `Tests` trailer, the generated
 * JavaScript it changes against `Output`, the marshaled formats it bumps,
 * and what the repository's rules require of a change to the v0 PPX bridge
 * and of an error fixture.
 * @param {CommitRecord} r
 * @returns {[boolean, string][]}
 */
export function checks(r) {
  /** @type {[boolean, string][]} */
  const out = [];
  const has = (/** @type {RegExp} */ re) => r.files.some(f => re.test(f.path));
  const plural = (/** @type {number} */ n, /** @type {string} */ w) =>
    `${n} ${w}${n === 1 ? "" : "s"}`;

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

  const missing = ["Area", "Output", "Tests"].filter(k => !r.trailers.has(k));
  if (missing.length)
    out.push([
      false,
      `No ${missing.join(", ")} trailer${missing.length === 1 ? "" : "s"}`,
    ]);

  const tests = r.trailers.get("Tests")?.replace(/\n/g, " ");
  const testFiles = r.files.filter(f => isTestFile(f.path)).length;
  if (tests === undefined) {
    // a missing Tests trailer is reported with the other required trailers
  } else if (/^none\b/.test(tests)) {
    if (testFiles)
      out.push([
        false,
        `Tests: none, but the diff changes ${plural(testFiles, "test file")}`,
      ]);
    else if (!/^none, because \S/.test(tests))
      out.push([false, "Tests: none gives no reason (none, because …)"]);
    else out.push([true, "No test changes, as recorded"]);
  } else {
    const listed = testItems(tests);
    const missing = listed.filter(
      t => !r.files.some(f => f.path.startsWith(t.path)),
    );
    out.push(
      missing.length
        ? [
            false,
            `Tests lists ${missing.map(t => t.path).join(", ")}, which the diff does not change`,
          ]
        : [true, "The tests listed are in the diff"],
    );
    const unlisted = r.files.filter(
      f =>
        isTestFile(f.path) &&
        !isSnapshot(f.path) &&
        !listed.some(t => f.path.startsWith(t.path)),
    );
    out.push(
      unlisted.length
        ? [
            false,
            `Tests omits test files the diff changes: ${unlisted.map(f => `${{ A: "+", D: "-" }[f.status] ?? "~"}${f.path}`).join(" ")}`,
          ]
        : [true, "Every test file in the diff is listed"],
    );
  }

  const changed = r.files.filter(
    f => isGeneratedJs(f.path) && f.status !== "A",
  ).length;
  const added = r.files.filter(
    f => isGeneratedJs(f.path) && f.status === "A",
  ).length;
  const output = r.trailers.get("Output");
  const js = `${changed ? plural(changed, "committed generated JS file") : "No committed generated JS file"} modified${added ? `, ${added} added` : ""}`;
  if (output !== undefined && /^unchanged/.test(output) && changed)
    out.push([false, `Output: unchanged, but ${js}`]);
  else out.push([true, js]);

  const bumped = [
    ...r.patch.matchAll(/^\+(?:let|and) (\w+)_magic_number = "([^"]+)"/gm),
  ].map(m => `${m[1]} ${m[2]}`);
  if (bumped.length)
    out.push([true, `Magic numbers bumped: ${bumped.join(", ")}`]);
  else if (has(/^compiler\/ml\/(parsetree|typedtree|types|cmt_format)\.mli?$/))
    out.push([
      false,
      "A marshaled type (parsetree, typed tree, types, cmt) changes with no magic number bump",
    ]);
  if (has(/^compiler\/ml\/parsetree0\.ml$/))
    out.push([
      false,
      "Changes the frozen v0 parsetree (compiler/ml/parsetree0.ml)",
    ]);
  if (has(/^compiler\/ml\/ast_mapper_(from|to)0\.ml$/))
    out.push([
      has(
        /^tests\/syntax_tests\/data\/ast-mapping\/|^tests\/ounit_tests\/ounit_ast_mapper0/,
      ),
      "The v0 PPX bridge changes; an ast-mapping fixture or bridge unit test is in the diff",
    ]);
  const fixtures = r.files.filter(
    f =>
      /^tests\/build_tests\/super_errors[^/]*\/fixtures\//.test(f.path) &&
      f.status !== "M",
  ).length;
  if (fixtures)
    out.push([
      has(/^tests\/ERROR_VARIANTS\.md$/),
      `${plural(fixtures, "super_errors fixture")} added or removed; tests/ERROR_VARIANTS.md ${has(/^tests\/ERROR_VARIANTS\.md$/) ? "updated" : "unchanged"}`,
    ]);
  return out;
}

/**
 * The bullets of a fragment, without their "- " and with continuation lines
 * joined.
 * @param {string} text
 */
export const bullets = text =>
  text
    .split(/\n(?=- )/)
    .map(b => b.replace(/^-\s+/, "").replace(/\n\s+/g, " "))
    .filter(Boolean);

// ---------------------------------------------------------------------------
// Cards

/** The changelog categories of changes/README.md, in CHANGELOG.md order. */
/** @type {Record<string, [string, string]>} */
const CATEGORY = {
  breaking: ["💥", "Breaking change"],
  feature: ["🚀", "New feature"],
  fix: ["🐛", "Bug fix"],
  docs: ["📝", "Documentation"],
  polish: ["💅", "Polish"],
  internal: ["🏠", "Internal"],
  none: ["", "No changelog"],
};

/** @param {string} category */
const pill = category => {
  const [emoji, label] = CATEGORY[category] ?? ["", category];
  return `<span class="pill cat-${esc(category)}">${emoji ? `${emoji} ` : ""}${esc(label)}</span>`;
};

/**
 * Splits on "; " outside parentheses: the items of a Measured trailer.
 * @param {string} v
 */
function items(v) {
  /** @type {string[]} */
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

/**
 * A state trailer: its leading word (`unchanged`, `changed`, `none`) as a
 * chip, the rest as prose.
 * @param {string} v
 */
const stateChip = v => {
  const [, word, rest] = /^([^\s,]+),?\s*(.*)$/s.exec(v) ?? ["", v, ""];
  const cls = /^(unchanged|none)$/.test(word) ? "same" : "diff";
  return `<div class="row"><span class="state ${cls}">${esc(word)}</span>${rest ? `<span>${inline(rest)}</span>` : ""}</div>`;
};

/**
 * The test suites of the repository, by the directory a test is in; the
 * directory heads the suite's group.
 * @type {[RegExp, string][]}
 */
const SUITES = [
  [/^tests\/syntax_tests\/data\//, "syntax"],
  [/^tests\/syntax_tests\//, "syntax"],
  [/^tests\/tests\/src\//, "integration"],
  [/^tests\/ounit_tests\//, "ounit"],
  [/^tests\/build_tests\/super_errors[^/]*\//, "super_errors"],
  [/^tests\/build_tests\//, "build"],
  [/^tests\/analysis_tests\//, "analysis"],
  [/^tests\/gentype_tests\//, "gentype"],
  [/^tests\/tools_tests\//, "rescript-tools"],
  [/^tests\/docstring_tests\//, "docstrings"],
  [/^rewatch\/tests\//, "rewatch integration"],
  [/^rewatch\/src\//, "cargo test"],
];

/**
 * A `Tests` trailer that lists tests, grouped by suite.
 * @param {string} value
 */
function testList(value) {
  /** @type {Map<string, string[]>} */
  const byGroup = new Map();
  /** @type {Record<string, [string, string]>} */
  const MARKS = { "+": ["add", "+"], "~": ["upd", "~"], "-": ["del", "−"] };
  for (const { mark, path } of testItems(value)) {
    const suite = SUITES.find(([re]) => re.test(path));
    const root = suite ? (suite[0].exec(path)?.[0] ?? "") : "";
    const label = `${suite?.[1] ?? "other"}${root ? ` <code>${esc(root)}</code>` : ""}`;
    const [cls, sign] = MARKS[mark] ?? ["", ""];
    byGroup.set(label, [
      ...(byGroup.get(label) ?? []),
      `<span class="test ${cls}" title="${esc(path)}">${sign ? `<b>${sign}</b> ` : ""}${esc(path.slice(root.length))}</span>`,
    ]);
  }
  return `<div class="tests">${[...byGroup]
    .map(
      ([label, ts]) =>
        `<div><span class="suite">${label}</span>${ts.join("")}</div>`,
    )
    .join("")}</div>`;
}

/** Trailers drawn by the card itself, or by the DCO app and git. */
const HIDDEN =
  /^(Area|PR|Changelog|Signed-off-by|Co-authored-by|Claude-Session)$/i;

/** Trailers shown as facts, in this order; any other trailer follows them. */
export const FACTS = ["Output", "Tests", "Measured"];

/**
 * One trailer as a fact: a state chip for `Output` and `none` tests, the
 * suites for listed tests, a list for `Measured`.
 * @param {string} key
 * @param {string} value
 */
function fact(key, value) {
  const one = value.replace(/\n/g, " ");
  const listed = items(one);
  const shown =
    key === "Tests" && !/^none\b/.test(one)
      ? testList(one)
      : /^(unchanged|changed|none)\b/.test(one)
        ? stateChip(one)
        : listed.length > 1
          ? `<ul class="run">${listed.map(l => `<li>${inline(l)}</li>`).join("")}</ul>`
          : value
              .split("\n")
              .map(line => `<span>${inline(line)}</span>`)
              .join("");
  return `<dt>${esc(key)}</dt><dd>${shown}</dd>`;
}

/** Sections drawn below the changes rather than among them. */
const BELOW = new Set(["Breaking", "Source", "Review"]);

/**
 * The words of a trailer: the layers of `Area`.
 * @param {CommitRecord} r
 * @param {string} key
 */
const trailerWords = (r, key) =>
  (r.trailers.get(key) ?? "").split(/\s+/).filter(Boolean);

/** @param {CommitRecord} r */
export function card(r) {
  const pr = (
    r.trailers.get("PR") ??
    /\(#(\d+)\)\s*$/.exec(r.subject)?.[1] ??
    ""
  ).replace(/\D/g, "");
  const subject = r.subject.replace(/\s*\(#\d+\)\s*$/, "");
  const categories = [...new Set(r.fragments.map(f => f.category))];
  const breaking = r.sections.filter(s => s.name === "Breaking");
  const joined = (/** @type {Section} */ s) =>
    [...s.text, ...s.block.map(l => l.trim())].join(" ").trim();
  const out = [
    `<article class="card${breaking.length || categories.includes("breaking") ? " breaking" : ""}" id="c-${esc(r.sha)}">`,
    `<header><div class="meta"><code>${esc(r.sha)}</code> · ${esc(r.author)}${r.date ? ` · ${esc(r.date)}` : ""}${
      pr ? ` · <a href="${REPO_URL}/pull/${pr}">#${pr}</a>` : ""
    }${r.noted ? ' · <span title="record from refs/notes/records">note</span>' : ""}</div><h3>${inline(subject)}</h3><div class="chips">${categories
      .map(pill)
      .join("")}${trailerWords(r, "Area")
      .map(a => `<span class="chip">${esc(a)}</span>`)
      .join("")}</div></header>`,
  ];
  out.push(drawSections(r.sections.filter(s => !BELOW.has(s.name))));
  for (const s of r.sections.filter(s => s.name === "Source"))
    out.push(`<p class="source"><b>Source</b> ${inline(joined(s))}</p>`);

  const below = [];
  for (const s of breaking)
    below.push(
      `<div class="banner"><strong>Breaking</strong> ${inline(joined(s))}</div>`,
    );
  for (const f of r.fragments)
    below.push(
      `<div class="entry">${pill(f.category)}<span>${bullets(f.text).map(inline).join("<br>")}</span></div>`,
    );
  if (!r.fragments.length)
    below.push(`<div class="entry">${pill("none")}</div>`);
  const reviews = r.sections.filter(s => s.name === "Review");
  if (reviews.length)
    below.push(
      `<h4>Review</h4><ul class="review">${reviews
        .map(s => `<li>${inline(joined(s))}</li>`)
        .join("")}</ul>`,
    );
  const keys = [
    ...FACTS.filter(k => r.trailers.has(k)),
    ...[...r.trailers.keys()].filter(
      k => !FACTS.includes(k) && !HIDDEN.test(k),
    ),
  ];
  const facts = keys.map(k => fact(k, r.trailers.get(k) ?? ""));
  if (facts.length) below.push(`<dl class="facts">${facts.join("")}</dl>`);
  below.push(
    `<ul class="checks">${checks(r)
      .map(([ok, t]) => `<li class="${ok ? "ok" : "warn"}">${esc(t)}</li>`)
      .join("")}</ul>`,
  );
  out.push(`<div class="below">${below.join("\n")}</div>`);

  if (r.files.length) {
    const plus = r.files.reduce((n, f) => n + f.added, 0);
    const minus = r.files.reduce((n, f) => n + f.removed, 0);
    out.push(
      `<details><summary>${r.files.length} file${r.files.length === 1 ? "" : "s"}, <span class="plus">+${plus}</span> <span class="minus">−${minus}</span></summary><ul class="files">${r.files
        .map(
          f =>
            `<li><span class="st st-${esc(f.status)}">${esc(f.status)}</span> <code>${esc(f.path)}</code> <span class="plus">+${f.added}</span> <span class="minus">−${f.removed}</span></li>`,
        )
        .join("")}</ul></details>`,
    );
  }
  if (r.coauthors.length)
    out.push(`<div class="meta">with ${esc(r.coauthors.join(" · "))}</div>`);
  out.push("</article>");
  return out.join("\n");
}

// ---------------------------------------------------------------------------
// Ranges and the page

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

/**
 * A bullet summary per range: its commits, the kinds of object they change,
 * its areas, the changelog categories of the fragments it leaves, its failed
 * checks; then a changelog draft from those fragments, and the files more
 * than one range (or commit) changes.
 * @param {Group[]} groups
 */
export function summary(groups) {
  const ranges = groups.filter(g => g.range);
  if (!ranges.length) return "";
  const list = ranges.map(g => {
    const kinds = new Set(
      g.records.flatMap(r =>
        r.sections
          .map(
            s =>
              /^([A-Z][A-Za-z]*) (?:added|modified|removed)\b/.exec(
                s.name,
              )?.[1] ?? "",
          )
          .filter(Boolean),
      ),
    );
    const areas = [...new Set(g.records.flatMap(r => trailerWords(r, "Area")))];
    /** @type {Map<string, number>} */
    const cats = new Map();
    for (const f of rangeFragments(g))
      cats.set(f.category, (cats.get(f.category) ?? 0) + 1);
    if (!cats.size) cats.set("none", 1);
    const failed = g.records.reduce(
      (n, r) => n + checks(r).filter(([ok]) => !ok).length,
      0,
    );
    const facts = [
      `${g.records.length} commit${g.records.length === 1 ? "" : "s"}`,
      kinds.size
        ? `changes: ${[...kinds].map(k => `<span class="chip">${esc(k)}</span>`).join(" ")}`
        : "",
      areas.length
        ? `areas: ${areas.map(a => `<span class="chip">${esc(a)}</span>`).join(" ")}`
        : "",
      `changelog: ${[...cats].map(([c, n]) => `${pill(c)}${n > 1 ? ` ×${n}` : ""}`).join(" ")}`,
      failed
        ? `<span class="warn-n">${failed} failed check${failed === 1 ? "" : "s"}</span>`
        : "no failed checks",
    ].filter(Boolean);
    return `<li><a href="#g-${esc(g.name)}"><code>${esc(g.range ?? g.name)}</code></a><ul>${facts.map(f => `<li>${f}</li>`).join("")}</ul></li>`;
  });
  /** @type {Map<string, string[]>} */
  const draft = new Map();
  for (const g of ranges)
    for (const f of rangeFragments(g))
      draft.set(f.category, [
        ...(draft.get(f.category) ?? []),
        ...bullets(f.text),
      ]);
  const order = Object.keys(CATEGORY);
  /** @type {Map<string, Set<string>>} */
  const owners = new Map();
  for (const g of ranges)
    for (const r of g.records)
      for (const f of r.files)
        owners.set(
          f.path,
          (owners.get(f.path) ?? new Set()).add(
            ranges.length > 1 ? g.name : r.sha,
          ),
        );
  const shared = [...owners].filter(([, os]) => os.size > 1).sort();
  return (
    `<section><h2>Summary</h2><ul class="summary">${list.join("")}</ul>` +
    (draft.size
      ? `<h4>Changelog draft</h4><div class="draft">${[...draft]
          .sort((a, b) => order.indexOf(a[0]) - order.indexOf(b[0]))
          .map(
            ([c, es]) =>
              `<div>${pill(c)}<ul>${es.map(e => `<li>${inline(e)}</li>`).join("")}</ul></div>`,
          )
          .join("")}</div>`
      : "") +
    (shared.length
      ? `<h4>Files changed by more than one ${ranges.length > 1 ? "range" : "commit"}</h4><ul class="summary">${shared
          .map(
            ([f, os]) =>
              `<li><code>${esc(f)}</code>: ${[...os].map(o => inline(o)).join(", ")}</li>`,
          )
          .join("")}</ul>`
      : "") +
    `</section>`
  );
}

/**
 * The page's title: the pull request's number and title when given, then
 * "Semantic Diff".
 * @param {{ pr?: string, title?: string }} heading
 */
export const pageTitle = ({ pr = "", title = "" }) =>
  pr || title
    ? `${pr ? `#${pr} ` : ""}${title} · Semantic Diff`
    : "Semantic Diff";

/**
 * The page's body: a heading (the pull request's number, linked, and its
 * title, when given), a summary of each range, then each range's cards.
 * @param {Group[]} groups
 * @param {{ pr?: string, title?: string }} [heading]
 */
export function pageBody(groups, { pr = "", title = "" } = {}) {
  pageShas.clear();
  for (const g of groups) for (const r of g.records) pageShas.add(r.sha);
  const commits = groups.reduce((n, g) => n + g.records.length, 0);
  const h1 =
    pr || title
      ? `<p class="kicker">Semantic <span>Diff</span></p><h1>${pr ? `<a href="${REPO_URL}/pull/${esc(pr)}">#${esc(pr)}</a> ` : ""}${inline(title)}</h1>`
      : `<h1 class="brand">Semantic <span>Diff</span></h1>`;
  return `<main><header>${h1}<p class="lede">${commits} commit${commits === 1 ? "" : "s"}, drawn from ${commits === 1 ? "its record" : "their records"}.</p></header>
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
}

// ---------------------------------------------------------------------------
// Styles: one sheet for the page; `:root` and `body` rules can be rescoped
// to a shadow root's host and page element.

const LIGHT = `--bg:#f5f4f1; --surface:#fff; --ink:#1c1b1a; --muted:#68625c; --rule:#dfdad4; --accent:#c8373c;
  --code-bg:#f1eee9; --math-bg:#f6f2ea; --k:#9b2a8a; --s:#277a3e; --n:#ad5a0c; --c:#8b857f; --t:#22609f; --a:#86600a; --p:#6f43b0;
  --add:#e1f2e4; --del:#f9e3e1; --ok:#25764a; --ok-bg:#e1f1e6; --warn:#955a05; --warn-bg:#fbefd8; --why:#7a560f; --why-bg:#faf3e3;
  --same:#5a6470; --same-bg:#eceff2; --diff:#1d5fa8; --diff-bg:#e3edf9;`;
const DARK = `--bg:#141312; --surface:#1d1b1a; --ink:#ebe7e3; --muted:#a29b95; --rule:#36312e; --accent:#f2777b;
  --code-bg:#24211f; --math-bg:#25221d; --k:#e08fd2; --s:#91d3a2; --n:#efb26e; --c:#8b857f; --t:#8db9ee; --a:#ddbd6d; --p:#c4a4ef;
  --add:#1b3223; --del:#3a1f1d; --ok:#8fd3a2; --ok-bg:#1b3223; --warn:#efc06d; --warn-bg:#3a2d16; --why:#ddbd6d; --why-bg:#2b2416;
  --same:#b0b8c2; --same-bg:#2a2e33; --diff:#8db9ee; --diff-bg:#1d2b3d; color-scheme:dark;`;

export const CSS = `
:root { ${LIGHT}
  --display:"Fraunces",Georgia,serif; --body:"Inter","Helvetica Neue",Arial,sans-serif; --mono:"JetBrains Mono",Menlo,Consolas,monospace;
  --math:"STIX Two Text","STIX Two Math","STIXGeneral","Cambria Math","Apple Symbols",Georgia,serif; }
@media (prefers-color-scheme: dark) { :root:not([data-theme="light"]) { ${DARK} } }
:root[data-theme="dark"] { ${DARK} }
* { box-sizing:border-box } body { margin:0; background:var(--bg); color:var(--ink); font:15px/1.6 var(--body) }
main { max-width:1080px; margin:0 auto; padding:32px 16px 64px; display:flex; flex-direction:column; gap:22px }
h1 { font:600 30px/1.2 var(--display); margin:0; text-wrap:balance } h1.brand { font-size:34px } h1 span, .kicker span { color:var(--accent) } h1 a { text-decoration:none }
.kicker { font:600 13px/1 var(--display); letter-spacing:.04em; margin:0 0 8px; color:var(--muted) } h2 { font:600 22px/1.2 var(--display); margin:12px 0 8px; padding-top:12px; border-top:2px solid var(--ink) }
h3 { font:600 20px/1.3 var(--display); margin:4px 0 6px; text-wrap:balance }
h4 { font:600 11px/1 var(--body); letter-spacing:.08em; text-transform:uppercase; color:var(--muted); margin:12px 0 4px }
p { margin:2px 0; max-width:75ch } .lede, .none { color:var(--muted) }
a { color:var(--accent) } :focus-visible { outline:2px solid var(--accent); outline-offset:2px }
code { font:12.5px/1.5 var(--mono); background:var(--code-bg); padding:1px 4px; border-radius:3px; overflow-wrap:anywhere }
.card { background:var(--surface); border:1px solid var(--rule); border-radius:10px; padding:20px 22px; display:flex; flex-direction:column; gap:4px; min-width:0 }
.card.breaking { border-top:4px solid var(--accent) }
.meta { font:12px/1.5 var(--mono); color:var(--muted) } .meta code { background:none; padding:0 }
.chips { display:flex; flex-wrap:wrap; gap:6px }
.chip { font:500 12px/1 var(--body); border:1px solid var(--rule); border-radius:4px; padding:4px 7px }
.pill { font:600 11.5px/1 var(--body); padding:5px 9px; border-radius:99px; white-space:nowrap; background:var(--code-bg); color:var(--muted) }
.pill.cat-breaking { background:var(--accent); color:#fff } .pill.cat-fix { background:var(--warn-bg); color:var(--warn) } .pill.cat-feature { background:var(--ok-bg); color:var(--ok) } .pill.cat-polish { background:var(--diff-bg); color:var(--diff) }
section.block { min-width:0 }
h4.change { display:flex; align-items:center; gap:8px; flex-wrap:wrap; text-transform:none; letter-spacing:0; font:600 14px/1.3 var(--body); color:var(--ink); margin:16px 0 6px }
h4.change .target { font-weight:400; color:var(--muted) }
.k-why h4.change { font:600 11px/1 var(--body); letter-spacing:.08em; text-transform:uppercase; color:var(--why); margin-top:8px }
.k-why .prose { background:var(--why-bg); border-left:3px solid var(--why); border-radius:6px; padding:8px 14px }
.act { font:600 10.5px/1 var(--body); letter-spacing:.06em; text-transform:uppercase; padding:4px 7px; border-radius:4px }
.act.added { color:var(--ok); background:var(--ok-bg) } .act.removed { color:var(--accent); background:var(--del) } .act.modified { color:var(--warn); background:var(--warn-bg) }
.prose p { margin:4px 0 }
pre { margin:4px 0; padding:10px 0; border-radius:6px; overflow-x:auto; background:var(--code-bg); font:12.5px/1.6 var(--mono) }
.ln { display:block; min-height:1.6em; white-space:pre-wrap; overflow-wrap:anywhere; padding:0 14px 0 calc(14px + (var(--i, 0) + 2) * 1ch); text-indent:calc((var(--i, 0) + 2) * -1ch) }
.ln.add, .cell.add, .map.add { background:var(--add) } .ln.del, .cell.del, .map.del { background:var(--del) }
.k { color:var(--k) } .s { color:var(--s) } .n { color:var(--n) } .c { color:var(--c); font-style:italic } .t { color:var(--t) } .a { color:var(--a) } .p { color:var(--p) } .v { color:var(--t); font-style:italic }
.paren, .stamp, .branch, .tree .label { color:var(--c) } .stamp { font-size:.85em } pre var { color:var(--t) }
.versions { display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); gap:10px; margin:4px 0 }
.versions figure { margin:0; min-width:0; display:flex; flex-direction:column } .versions figure > :last-child { flex:1 }
.vlabel { font:600 11px/1.4 var(--body); letter-spacing:.06em; text-transform:uppercase; color:var(--muted); margin:0 0 4px }
.vlabel .cap { font:400 12px/1.4 var(--body); letter-spacing:0; text-transform:none }
.vlabel::before { content:""; display:inline-block; width:.8em; height:.8em; margin-right:5px; vertical-align:-1px; border-radius:2px; box-shadow:inset 0 0 0 1px var(--rule) }
.vlabel.del::before { background:var(--del) } .vlabel.add::before { background:var(--add) }
.pair { display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); column-gap:10px; margin:4px 0 }
.rows { grid-column:1 / -1; display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); column-gap:10px; font:12.5px/1.6 var(--mono) }
.rows > span { grid-row:var(--r); grid-column:var(--c); background:var(--code-bg) } .rows > .ln.add { background:var(--add) } .rows > .ln.del { background:var(--del) }
.rows > .gap { background:repeating-linear-gradient(135deg, var(--code-bg) 0 5px, transparent 5px 8px) }
.rows > .pad { height:10px } .rows > :nth-child(-n+2) { border-radius:6px 6px 0 0 } .rows > :nth-last-child(-n+2) { border-radius:0 0 6px 6px }
.rows.text > span { background:var(--surface); border-left:3px solid var(--accent) } .rows.text > .ln.add { background:var(--add) } .rows.text > .ln.del { background:var(--del) }
.lang { font:500 10px/1 var(--mono); border:1px solid var(--rule); border-radius:4px; padding:2px 5px; color:var(--muted) }
.text pre { background:var(--surface); border:1px solid var(--rule); border-left:3px solid var(--accent) } .text pre .ln { white-space:pre-wrap }
.rules { display:flex; flex-direction:column; gap:14px; background:var(--math-bg); border-left:3px solid var(--accent); border-radius:6px; padding:14px 18px; overflow-x:auto; font:16px/1.6 var(--math) }
.rules.add { box-shadow:inset 0 0 0 1px var(--ok) } .rules.del { box-shadow:inset 0 0 0 1px var(--accent) }
.rules sub, .rules sup { font-size:.72em; line-height:0 }
.formulas { display:flex; flex-direction:column; gap:4px } .formulas .row { display:flex; flex-wrap:wrap; gap:4px 28px } .formulas .row.cont { padding-left:2em }
.infer { display:flex; align-items:center; gap:12px } .frac { display:inline-flex; flex-direction:column; align-items:center; min-width:0 }
.premises { display:flex; flex-wrap:wrap; justify-content:center; gap:4px 26px; padding:0 6px 3px } .bar { align-self:stretch; border-top:1.5px solid var(--ink) }
.conclusion { padding:3px 6px 0; text-align:center } .rule-name { font:15px/1 var(--math); font-variant:small-caps; color:var(--accent); white-space:nowrap }
var { font-style:italic } .m-op { color:var(--t) } .m-con { color:var(--ok); font-variant:small-caps } .m-kw { color:var(--k); font-weight:600 } .m-bind { color:var(--k) }
.m-rel { color:var(--accent); padding:0 .12em } .m-lit { font:13px/1.5 var(--mono); color:var(--k) } .m-text { font:14px/1.5 var(--body); color:var(--muted) }
code.m-code { font:13px/1.5 var(--mono); background:var(--surface); border:1px solid var(--rule); padding:1px 5px; border-radius:4px }
.rewrites { display:flex; flex-direction:column; gap:8px }
.rewrite { display:grid; grid-template-columns:minmax(0,1fr) auto minmax(0,1fr); align-items:center; gap:10px } .rewrite pre { height:100% }
.to { color:var(--accent); font:600 20px/1 var(--math) }
.cond { display:flex; gap:10px; align-items:baseline; padding-left:1em; font:15px/1.5 var(--math) }
.cond .label { font:600 11px/1 var(--body); letter-spacing:.05em; text-transform:uppercase; color:var(--muted); min-width:5em }
.matrix { display:grid; gap:2px 18px; background:var(--code-bg); border-radius:6px; padding:10px 14px; overflow-x:auto; justify-content:start }
.matrix .cell { background:none; padding:0 4px; white-space:pre }
.mapping { display:flex; flex-direction:column; gap:4px; background:var(--math-bg); border-left:3px solid var(--accent); border-radius:6px; padding:10px 14px }
.map { display:flex; flex-wrap:wrap; gap:6px 12px; align-items:baseline } .map code { background:var(--surface); border:1px solid var(--rule) }
ul.effect { margin:4px 0; padding-left:20px } ul.effect li { margin:3px 0 } .arrow { color:var(--accent); font-weight:600 }
.below { display:flex; flex-direction:column; gap:6px; margin-top:12px; padding-top:10px; border-top:1px solid var(--rule) }
.banner { background:var(--del); border-left:4px solid var(--accent); padding:8px 14px; border-radius:6px } .banner strong { color:var(--accent); margin-right:6px; text-transform:uppercase; font-size:12px; letter-spacing:.06em }
.entry { display:flex; gap:10px; align-items:baseline; flex-wrap:wrap; font-size:14px } .entry > span:last-child:not(.pill) { flex:1 1 300px; min-width:0 }
.source { font-size:12.5px; color:var(--muted) } .source b { font-weight:600; font-size:11px; letter-spacing:.06em; text-transform:uppercase; margin-right:4px }
dl.facts { display:grid; grid-template-columns:max-content minmax(0,1fr); gap:6px 16px; margin:4px 0 0; font-size:13.5px }
dt { font:600 11px/2 var(--body); letter-spacing:.06em; text-transform:uppercase; color:var(--muted) } dd { margin:0; min-width:0 } dd ul { margin:0; padding-left:18px }
dd .row { display:flex; flex-wrap:wrap; gap:6px; align-items:baseline }
.state { padding:2px 8px; border-radius:5px } .state.same { background:var(--same-bg); color:var(--same) } .state.diff { background:var(--diff-bg); color:var(--diff) }
.delta { font-variant-numeric:tabular-nums; white-space:nowrap } .delta .from { color:var(--muted); text-decoration:line-through } .delta .to { font-weight:600; color:var(--ok) }
.tests { display:flex; flex-direction:column; gap:4px } .tests > div { display:flex; flex-wrap:wrap; gap:4px; align-items:baseline }
.suite { font:600 11px/1.6 var(--body); color:var(--muted); min-width:9em } .suite code { font-size:11px; font-weight:400; background:none; padding:0 }
.test { font:12px/1.3 var(--mono); padding:3px 7px; border-radius:4px; background:var(--code-bg); overflow-wrap:anywhere }
.test.add { background:var(--ok-bg); color:var(--ok) } .test.del { background:var(--del); color:var(--accent) } .test b { font-weight:600 }
ul.review, .run { margin:0; padding-left:18px } ul.review li, .run li { margin:2px 0 }
.draft { display:flex; flex-direction:column; gap:10px; background:var(--surface); border:1px solid var(--rule); border-radius:10px; padding:14px 16px } .draft ul { margin:6px 0 0; padding-left:18px }
ul.checks { list-style:none; margin:4px 0 0; padding:0; display:flex; flex-wrap:wrap; gap:6px; font-size:12.5px }
ul.checks li { padding:3px 9px; border-radius:5px } ul.checks li.ok { background:var(--ok-bg); color:var(--ok) } ul.checks li.ok::before { content:"✓ " }
ul.checks li.warn { background:var(--warn-bg); color:var(--warn) } ul.checks li.warn::before { content:"! " }
details { margin-top:8px; font-size:13px; color:var(--muted) } summary { cursor:pointer }
ul.files { list-style:none; margin:6px 0 0; padding:0 } .st { font:600 11px var(--mono) } .st-A, .plus { color:var(--ok) } .st-D, .minus { color:var(--accent) }
ul.summary { margin:4px 0; padding-left:20px } ul.summary li { margin:3px 0 } ul.summary ul { padding-left:18px; color:var(--muted) } .warn-n { color:var(--warn); font-weight:600 }
pre, code, .test, .rows { font-variant-ligatures:none }
@media (max-width:720px) { .versions, .rewrite, .rows { grid-template-columns:minmax(0,1fr) }
  .pair { display:block } .pair > .vlabel { display:inline-block; margin-right:14px } .rows > span { grid-row:auto; grid-column:1 } .rows > .dup, .rows > .gap { display:none } .rewrite .to { justify-self:center } dl.facts { grid-template-columns:minmax(0,1fr) } .card { padding:16px } }`;
