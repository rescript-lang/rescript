// @ts-check

/* Commit records, parsed from a commit message and drawn as HTML. The
 * caller (commitRecords.js) reads each commit's identity and message from
 * git. The
 * format is the `records` skill's (.claude/skills/records/SKILL.md): a
 * subject, a `Why:` section, object blocks such as
 * `AST modified (raw extensions):`, each followed by an optional `Example:`
 * program and its forms as `Before (<form>):` and `After (<form>):` blocks,
 * `Review:` and `Breaking:` sections, and the trailers.
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
// The forms a record writes an object in, and how each is drawn

/**
 * How a form is drawn.
 * @typedef {"math" | "tree" | "matrix" | "mapping" | "rescript" | "js" | "lambda" | "text" | "report"} Drawing
 */

/**
 * Each form's drawing, by the name a `Before (<form>):` or `After (<form>):`
 * block gives it: a compiler's printed tree or code, a diagnostic, or a
 * hand-written typing rule, clause matrix or representation mapping, or
 * reanalyze's report. A form
 * of another name is drawn as text.
 * @type {Record<string, Drawing>}
 */
const FORMS = {
  parse: "tree",
  expanded: "tree",
  typed: "tree",
  lambda: "lambda",
  js: "js",
  rescript: "rescript",
  diagnostic: "text",
  text: "text",
  rule: "math",
  matrix: "matrix",
  representation: "mapping",
  reanalyze: "report",
};

/**
 * An object block's heading: `<Kind> <added|modified|removed> (<object>)`,
 * the kind one word, the object's parentheses optional.
 */
const BLOCK =
  /^([A-Z][A-Za-z]*) (added|modified|removed)(?: \(((?:[^()]|\([^()]*\))*)\))?$/;

/** A section an object block holds: its example or one version of a form. */
const PART = /^(?:Example|(Before|After) \(([a-z]+)\))$/;

// ---------------------------------------------------------------------------
// Records

/**
 * @typedef {{ name: string, text: string[], block: string[] }} Section
 * @typedef {{ sha: string, author: string, date: string, message: string }} CommitInfo
 * @typedef {CommitInfo & {
 *   subject: string, sections: Section[], coauthors: string[]
 * }} CommitRecord
 * @typedef {{ range?: string, records: CommitRecord[] }} Group
 */

/** The sections a message names besides object blocks and their parts. */
const NAMED = new Set(["Why", "Review", "Breaking"]);

/**
 * A trailer line: `Key: value`, the key one word of letters, digits and
 * hyphens other than a section's name.
 * @param {string} l
 */
const isTrailer = l => {
  const key = /^([A-Za-z][A-Za-z0-9-]*):\s+\S/.exec(l)?.[1];
  return key !== undefined && !NAMED.has(key) && key !== "Example";
};

/**
 * The line a message's trailers start at: the first of its last lines that
 * are trailers or lines continuing one (starting with a space), the run
 * starting at a trailer.
 * @param {string[]} lines
 */
function trailerStart(lines) {
  let start = lines.length;
  for (let k = lines.length - 1; k > 0; k--) {
    if (isTrailer(lines[k])) start = k;
    else if (!/^\s+\S/.test(lines[k])) break;
  }
  return start;
}

/**
 * A line with the tabs of its indentation expanded to stops 4 columns apart.
 * @param {string} l
 */
const untab = l =>
  l.replace(/^[ \t]+/, ws => {
    let col = 0;
    for (const c of ws) col = c === "\t" ? col + 4 - (col % 4) : col + 1;
    return " ".repeat(col);
  });

/** Whether a line is indented by 4 spaces or more. @param {string} l */
const isIndented = l => /^ {4,}\S/.test(l);

/**
 * A record's subject, sections and trailers. A section starts at a line
 * `Name:`, where the name is an object block's heading, a version heading,
 * `Why`, `Review` or `Breaking`, or `Example` alone on its line; it holds
 * the text after its colon and the lines indented by 4 spaces under it. A
 * blank line ends a section that holds only text. Unnamed sections hold the
 * paragraphs and indented blocks between.
 * @param {string} text
 */
function parseMessage(text) {
  const lines = text.trimEnd().split("\n").map(untab);
  const end = trailerStart(lines);
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
    const head = /^(\S(?:[^:]|\([^()]*\))*):(?:\s+(.*))?$/.exec(line);
    const name = head?.[1] ?? "";
    if (
      head &&
      (BLOCK.test(name) ||
        (PART.test(name) && !(name === "Example" && head[2])) ||
        NAMED.has(name))
    ) {
      current = { name, text: head[2] ? [head[2]] : [], block: [] };
      sections.push(current);
    } else if (isIndented(line)) {
      current ??= prose();
      current.block.push(line);
    } else if (line.trim()) {
      if (!current || current.block.length) current = prose();
      current.text.push(line.trim());
    } else if (current?.block.length) current.block.push("");
    else if (current && (!current.name || current.text.length))
      current = undefined;
  }
  return { subject: lines[0], sections, trailers: lines.slice(end) };
}

/**
 * A commit's record: its identity and its message, parsed; the co-authors
 * are the names its `Co-Authored-By:` trailers give.
 * @param {CommitInfo} commit
 * @returns {CommitRecord}
 */
export function parseRecord(commit) {
  const { subject, sections, trailers } = parseMessage(commit.message);
  const coauthors = trailers
    .filter(l => /^Co-Authored-By:/i.test(l))
    .map(l => l.replace(/^Co-Authored-By:\s*/i, "").replace(/\s*<[^>]*>/, ""));
  return { ...commit, subject, sections, coauthors: [...new Set(coauthors)] };
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

/** @type {{ [L in "rescript" | "js" | "lambda"]: Lang }} */
const LANG = {
  rescript: {
    token:
      /(?<c>\/\/[^\n]*|\/\*[\s\S]*?\*\/)|(?<s>"(?:\\.|[^"\\\n])*"|`(?:\\.|[^`\\])*`|'(?:\\u\{[0-9a-fA-F]+\}|\\.|[^'\\\n])')|(?<a>@{1,2}[A-Za-z_][\w.]*|%{1,2}[A-Za-z_][\w.]*)|(?<p>#[A-Za-z_]\w*)|(?<n>\b\d[\d_]*(?:\.\d+)?(?:[eE][+-]?\d+)?n?\b|\b0[xob][0-9a-fA-F_]+\b)|(?<v>'[a-z]\w*)|(?<i>[A-Za-z_][\w']*)/g,
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
  lambda: {
    token:
      /(?<s>"(?:\\.|[^"\\\n])*")|(?<p>#[A-Za-z_][\w.]*)|(?<i>[A-Za-z_][\w$.']*)(?<stamp>\/\d+)?|(?<n>-?\b\d+(?:\.\d+)?[nl]?\b)|(?<paren>[()])/g,
    keywords: words(
      "let letrec function if switch stringswitch staticcatch staticraise catch exit try with while for seq apply makeblock field setfield default case",
    ),
  },
};

/**
 * Code highlighted by token: comments, strings, attributes and extensions,
 * polymorphic variants, numbers, keywords, capitalised names (modules,
 * constructors), type variables; in Lambda, primitives (`#is_not_none`),
 * identifier stamps (`x/1002`) and parentheses.
 * @param {string} code
 * @param {keyof typeof LANG} lang
 */
function highlight(code, lang) {
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
        : /^[A-Z]/.test(id)
          ? span("t", id)
          : esc(id);
      if (g.stamp) out += span("stamp", g.stamp);
    } else {
      const cls = ["c", "s", "a", "p", "n", "v", "paren"].find(
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
 * The rows of a Before/After pair drawn side by side: a line common to both
 * versions is one row; between two common lines, the lines only Before has
 * and the lines only After has share rows, the shorter side padded with
 * gaps (`null`). Lines compare with their indentation, so a line that moves
 * to another depth is a change.
 * @param {string[]} a
 * @param {string[]} b
 * @returns {{ a: number | null, b: number | null, same: boolean }[]}
 */
function alignLines(a, b) {
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
 * The lines or parts of one version of a Before/After pair that the other
 * lacks, and their shading.
 * @typedef {{ marked?: Set<number>, mark?: "add" | "del" }} Marks
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

/** An elision line, `…` and any text, marking lines left out. @param {string} l */
const elided = l => l.trimStart().startsWith("…");

/**
 * Lines of code, each drawn by `draw`, the marked ones shaded and the
 * elisions muted.
 * @param {string[]} lines
 * @param {(code: string) => string} draw
 * @param {Marks} [marks]
 */
function codeLines(lines, draw, marks = {}) {
  return `<pre class="code">${codeHtml(lines, draw)
    .map((l, i) =>
      line(
        l,
        indentOf(lines[i] ?? ""),
        elided(lines[i] ?? "")
          ? "elide"
          : marks.marked?.has(i)
            ? marks.mark
            : "",
      ),
    )
    .join("")}</pre>`;
}

/** A note line of a drawing: `label: text`. */
const NOTE = /^\s*[a-z][\w ]{0,20}:\s/;

/**
 * The parts of a figure (a typing rule or a rewrite) that a Before/After
 * pair compares: each part's text and its slot, the place it holds in the
 * figure. Parts compare with the parts of the other version in their slot.
 * @typedef {{ slot: string, text: string }} Atom
 */

/**
 * The parts of `a` and of `b` outside a longest common subsequence of the
 * parts in each slot: the parts a Before/After pair of figures changes.
 * @param {Atom[]} a
 * @param {Atom[]} b
 * @returns {[Set<number>, Set<number>]}
 */
function atomDiff(a, b) {
  const del = new Set();
  const add = new Set();
  for (const slot of new Set([...a, ...b].map(x => x.slot))) {
    const ia = a.flatMap((x, n) => (x.slot === slot ? [n] : []));
    const ib = b.flatMap((x, n) => (x.slot === slot ? [n] : []));
    const pairs = matchLines(
      ia.map(n => a[n].text),
      ib.map(n => b[n].text),
      t => t.trim(),
    );
    const ka = new Set(pairs.map(p => p[0]));
    const kb = new Set(pairs.map(p => p[1]));
    for (const [k, n] of ia.entries()) if (!ka.has(k)) del.add(n);
    for (const [k, n] of ib.entries()) if (!kb.has(k)) add.add(n);
  }
  return [del, add];
}

/**
 * A group of code lines: plain code, or a rewrite, its terms in order
 * (`a ⇝ b ⇝ c`) and its side conditions.
 * @typedef {{ plain: string[] } | { terms: string[][], conds: string[] }} CodeGroup
 */

/**
 * The groups of code separated by blank lines. In a group holding `⇝`, each
 * `⇝` separates the code before it from the code after it (`lhs ⇝ rhs` on
 * one line, or `⇝` alone on a line), and the `label: text` lines after the
 * last term are side conditions.
 * @param {string[]} lines
 * @returns {CodeGroup[]}
 */
function codeGroups(lines) {
  return groups(lines).map(ls => {
    const last = ls.map(l => l.includes("⇝")).lastIndexOf(true);
    if (last < 0) return { plain: ls };
    const rest = ls.slice(last + 1);
    const cond = rest.findIndex(l => NOTE.test(l));
    const body = [
      ...ls.slice(0, last + 1),
      ...(cond < 0 ? rest : rest.slice(0, cond)),
    ];
    /** @type {string[][]} */
    const terms = [[]];
    for (const l of body)
      l.split("⇝").forEach((piece, i) => {
        if (i > 0) terms.push([]);
        const t = i === 0 ? piece.trimEnd() : piece.trim();
        if (t.trim()) terms[terms.length - 1].push(t);
      });
    return {
      terms: terms.map(dedent),
      conds: cond < 0 ? [] : rest.slice(cond),
    };
  });
}

/**
 * The parts of code a Before/After pair compares: each line of plain code,
 * each line of a rewrite's term in the slot of the term's position, each
 * side condition.
 * @param {string[]} lines
 * @returns {Atom[]}
 */
const codeAtoms = lines =>
  codeGroups(lines).flatMap(g =>
    "plain" in g
      ? g.plain.map(text => ({ slot: "plain", text }))
      : [
          ...g.terms.flatMap((t, i) =>
            t.map(text => ({ slot: `term${i}`, text })),
          ),
          ...g.conds.map(text => ({ slot: "cond", text })),
        ],
  );

/**
 * Code, or rewrites of code (`codeGroups`): each term drawn as code, `⇝`
 * between, the side conditions drawn as mathematics. Marked parts
 * (numbered as `codeAtoms` lists them) are shaded.
 * @param {string[]} lines
 * @param {keyof typeof LANG} lang
 * @param {Marks} [marks]
 */
function code(lines, lang, marks = {}) {
  const draw = (/** @type {string} */ c) => highlight(c, lang);
  if (!lines.some(l => l.includes("⇝"))) return codeLines(lines, draw, marks);
  let n = 0;
  /** The marks of the next `count` parts, numbered from 0. @param {number} count */
  const shade = count => {
    const from = n;
    n += count;
    return {
      marked: new Set(
        Array.from({ length: count }, (_, k) => k).filter(k =>
          marks.marked?.has(from + k),
        ),
      ),
      mark: marks.mark,
    };
  };
  return `<div class="rewrites">${codeGroups(lines)
    .map(g => {
      if ("plain" in g) return codeLines(g.plain, draw, shade(g.plain.length));
      const terms = g.terms
        .map(t => codeLines(t, draw, shade(t.length)))
        .join('<span class="to">⇝</span>');
      return (
        `<div class="rewrite" style="--n:${g.terms.length - 1}">${terms}</div>` +
        g.conds
          .map(l =>
            condition(l, shade(1).marked.size ? (marks.mark ?? "") : ""),
          )
          .join("")
      );
    })
    .join("")}</div>`;
}

/**
 * A side condition: `label: formula`, or a formula.
 * @param {string} l
 * @param {string} [mark]
 */
function condition(l, mark = "") {
  const m = /^\s*([a-z][\w ]{0,20}):\s+(.*)$/.exec(l);
  return `<div class="cond${mark ? ` ${mark}` : ""}">${m ? `<span class="label">${esc(m[1])}</span>` : ""}<span class="m">${math(m ? m[2] : l.trim())}</span></div>`;
}

// ---------------------------------------------------------------------------
// Inline text

/** Short shas of the cards on the page, for linking mentions. */
const pageShas = new Set();

/**
 * Prose: `code` as highlighted ReScript; `#1234` and full pull request and
 * issue URLs as `#1234` links; a 9-character short sha of a commit on the
 * page as a link to its card.
 * @param {string} s
 */
const inline = s =>
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
            .replace(/\b([0-9a-f]{9})\b/g, (m, sha) =>
              pageShas.has(sha)
                ? `<a href="#c-${sha}"><code>${sha}</code></a>`
                : m,
            ),
    )
    .join("");

// ---------------------------------------------------------------------------
// Mathematics: inference rules and formulas

/**
 * A formula's tokens: `code` in backticks, a sub- or superscript (`_x`,
 * `_{…}`, `^{…}`), a name, or any other character.
 * @param {string} text
 */
const mathTokens = text =>
  text.matchAll(
    /(?<code>`[^`]+`)|(?<script>_\{[^{}]*\}|\^\{[^{}]*\}|_[A-Za-z0-9Ͱ-Ͽ]+)|(?<name>[A-Za-zͰ-Ͽ][A-Za-z0-9Ͱ-Ͽ'′]*)|[\s\S]/g,
  );

/**
 * A formula as type theory sets it: one-letter names, with their primes,
 * italic variables; `_x`, `_{…}` and `^{…}` sub- and superscripts (`e_1` is
 * e with subscript 1); `code` in backticks highlighted ReScript.
 * @param {string} text
 * @returns {string}
 */
function math(text) {
  return [...mathTokens(text)]
    .map(m => {
      const g = m.groups ?? {};
      if (g.code)
        return `<code class="m-code">${highlight(g.code.slice(1, -1), "rescript")}</code>`;
      if (g.script) {
        const tag = g.script.startsWith("^") ? "sup" : "sub";
        return `<${tag}>${math(g.script.replace(/^[_^]\{?|\}$/g, ""))}</${tag}>`;
      }
      if (g.name && /^.[0-9'′]*$/.test(g.name))
        return `<var>${esc(g.name)}</var>`;
      return esc(m[0]);
    })
    .join("");
}

/** A line's parts: runs of three or more spaces separate side-by-side formulas. @param {string} line */
const parts = line =>
  line
    .trim()
    .split(/\s{3,}/)
    .filter(Boolean);

/**
 * The groups of a block's lines separated by blank lines.
 * @param {string[]} block
 */
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
 * A group of mathematics: an inference rule (premises, the rule's name,
 * the conclusion's lines) or rows of formulas.
 * @typedef {{ premises: string[], name: string, conclusion: string[] } | { rows: string[][] }} MathGroup
 */

/**
 * Mathematics: groups separated by blank lines; a group with a line of `─`
 * is an inference rule (premises above, conclusion below, the rule's name in
 * parentheses after the bar); any other group is a list of formulas.
 * @param {string[]} block
 * @returns {MathGroup[]}
 */
const mathGroups = block =>
  groups(block).map(g => {
    const bar = g.findIndex(l => /^\s*─{3,}/.test(l));
    if (bar < 0) return { rows: g.map(parts) };
    return {
      premises: g.slice(0, bar).flatMap(parts),
      name: /\(([^()]+)\)\s*$/.exec(g[bar])?.[1] ?? "",
      conclusion: g.slice(bar + 1).map(l => l.trim()),
    };
  });

/**
 * The parts of mathematics a Before/After pair compares, in the order they
 * are drawn: each premise, conclusion line and rule name, and each formula.
 * @param {string[]} block
 * @returns {Atom[]}
 */
const mathAtoms = block =>
  mathGroups(block).flatMap(g =>
    "rows" in g
      ? g.rows.flat().map(text => ({ slot: "formula", text }))
      : [
          ...g.premises.map(text => ({ slot: "premise", text })),
          ...g.conclusion.map(text => ({ slot: "conclusion", text })),
          ...(g.name ? [{ slot: "name", text: g.name }] : []),
        ],
  );

/**
 * Mathematics drawn (`mathGroups`): inference rules and rows of formulas,
 * the marked parts (numbered as `mathAtoms` lists them) shaded.
 * @param {string[]} block
 * @param {Marks} [marks]
 */
function rules(block, marks = {}) {
  let n = 0;
  /** The next part's class attribute. */
  const cls = (extra = "") => {
    const c = [extra, marks.marked?.has(n++) ? (marks.mark ?? "") : ""]
      .filter(Boolean)
      .join(" ");
    return c ? ` class="${c}"` : "";
  };
  const html = mathGroups(block).map(g => {
    if ("rows" in g)
      return `<div class="formulas">${g.rows
        .map(
          r =>
            `<div class="row">${r.map(p => `<span${cls()}>${math(p)}</span>`).join("")}</div>`,
        )
        .join("")}</div>`;
    const premises = g.premises
      .map(p => `<span${cls()}>${math(p)}</span>`)
      .join("");
    const conclusion = g.conclusion
      .map(l => `<div${cls()}>${math(l)}</div>`)
      .join("");
    return (
      `<div class="infer"><div class="frac"><div class="premises">${premises}</div><div class="bar"></div><div class="conclusion">${conclusion}</div></div>` +
      (g.name ? `<span${cls("rule-name")}>(${esc(g.name)})</span>` : "") +
      `</div>`
    );
  });
  return `<div class="rules">${html.join("")}</div>`;
}

// ---------------------------------------------------------------------------
// Trees, matrices, mappings

/**
 * Whether a line of a tree is a node: a nonblank line other than a lone `[`
 * or `]`, the list brackets of the compiler's tree printers. The elements
 * of a list stand one level deeper than the node owning it.
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
 * connectors: for each node, its line's index, its HTML and the column it
 * starts at. Depths count from the shallowest node, so a root drawn inside
 * the printer's outer `[` stands at depth 0, drawn bare.
 * @param {string[]} lines
 * @returns {{ i: number, html: string, indent: number }[]}
 */
function treeRows(lines) {
  const nodes = lines
    .map((l, i) => ({ i, level: indentOf(l) >> 1, l }))
    .filter(r => treeLine(r.l));
  const top = Math.min(...nodes.map(r => r.level));
  const rows = nodes.map(r => ({ ...r, depth: r.level - top }));
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
 * A tree, drawn with connectors.
 * @param {string[]} lines
 */
const tree = lines =>
  `<pre class="code tree">${treeRows(lines)
    .map(r => line(r.html, r.indent, elided(lines[r.i]) ? "elide" : ""))
    .join("")}</pre>`;

/**
 * The cells of a clause matrix's rows: columns three or more spaces apart.
 * @param {string} l
 */
const cellsOf = l => (NOTE.test(l) ? [] : parts(l));

/**
 * A clause matrix of the match compiler: one row a line, columns three or
 * more spaces apart, each cell a ReScript pattern; `label: text` lines are
 * notes below it.
 * @param {string[]} lines
 */
function matrix(lines) {
  const rows = lines.filter(l => l.trim());
  const cells = rows.filter(l => !NOTE.test(l));
  const width = Math.max(1, ...cells.map(l => parts(l).length));
  return (
    `<div class="matrix" style="grid-template-columns:repeat(${width},max-content)">${cells
      .map(l => {
        const ps = parts(l);
        return ps
          .concat(Array(width - ps.length).fill(""))
          .map(
            p => `<code class="cell">${highlight(p, "rescript") || " "}</code>`,
          )
          .join("");
      })
      .join("")}</div>` +
    rows
      .filter(l => NOTE.test(l))
      .map(l => condition(l))
      .join("")
  );
}

/**
 * The lines of a clause matrix for aligned rows: each cell padded to its
 * column's width in `widths`, the columns three spaces apart; a note drawn
 * as a side condition.
 * @param {string[]} lines
 * @param {number[]} widths
 */
const matrixLines = (lines, widths) =>
  lines.map(l =>
    NOTE.test(l)
      ? condition(l)
      : cellsOf(l)
          .map((p, c) => highlight(p.padEnd(widths[c] ?? 0), "rescript"))
          .join("   "),
  );

/**
 * A representation mapping's line split at its `↦`, or null for a note.
 * @param {string} l
 */
const mapsTo = l => {
  const at = l.indexOf("↦");
  return at < 0 ? null : [l.slice(0, at).trim(), l.slice(at + 1).trim()];
};

/**
 * How ReScript values are represented in JavaScript: `ReScript ↦ JavaScript`
 * lines, each side highlighted; `label: text` lines are conditions.
 * @param {string[]} lines
 */
function mapping(lines) {
  return `<div class="mapping">${lines
    .map(l => {
      if (!l.trim()) return "";
      const m = mapsTo(l);
      if (!m) return condition(l);
      return `<div class="map"><code>${highlight(m[0], "rescript")}</code><span class="to">↦</span><code>${highlight(m[1], "js")}</code></div>`;
    })
    .join("")}</div>`;
}

/**
 * The lines of a representation mapping for aligned rows: the ReScript side
 * padded to `width`, so that the arrows align; a note drawn as a side
 * condition.
 * @param {string[]} lines
 * @param {number} width
 */
const mappingLines = (lines, width) =>
  lines.map(l => {
    const m = mapsTo(l);
    if (!l.trim()) return "";
    if (!m) return condition(l);
    return `${highlight(m[0].padEnd(width), "rescript")} <span class="to">↦</span> ${highlight(m[1], "js")}`;
  });

// ---------------------------------------------------------------------------
// Reanalyze reports

/** A finding's location line: `File.res:3:5-11`. */
const LOCATION = /^\S+:\d+:\d+(?:-\d+(?::\d+)?)?$/;

/** An elision paragraph of a report: `…` or `...`, then any text. */
const REPORT_ELISION = /^\s*(?:…|\.\.\.)/;

/**
 * One entry of a reanalyze report: a finding (its analysis, location and
 * message lines), an elision marking findings left out, or any other
 * paragraph, such as the closing count. `key` is the entry's text, its
 * identity across versions.
 * @typedef {{ key: string, analysis?: string, location?: string, elision?: boolean, message: string[] }} ReportEntry
 */

/**
 * A reanalyze report as reanalyze prints it: paragraphs separated by blank
 * lines, each finding an analysis line, a location line and its message
 * lines. A paragraph starting with `…` or `...` is an elision.
 * @param {string[]} lines
 * @returns {ReportEntry[]}
 */
const reportEntries = lines =>
  groups(lines).map(g => {
    const ls = dedent(g);
    const key = ls.map(l => l.trimEnd()).join("\n");
    if (REPORT_ELISION.test(ls[0])) return { key, elision: true, message: ls };
    return ls.length >= 2 && LOCATION.test(ls[1].trim())
      ? {
          key,
          analysis: ls[0].trim(),
          location: ls[1].trim(),
          message: dedent(ls.slice(2)),
        }
      : { key, message: ls };
  });

/**
 * The analysis every finding of the reports names, when they all name the
 * same one, or "".
 * @param {string[][]} reports
 */
function sharedAnalysis(reports) {
  const names = new Set(
    reports.flatMap(r =>
      reportEntries(r).flatMap(e => (e.analysis ? [e.analysis] : [])),
    ),
  );
  return names.size === 1 ? [...names][0] : "";
}

/**
 * A finding's message: attributes such as `@throws(Not_found)` marked, and
 * the locations it cites muted.
 * @param {string} s
 */
const reportMessage = s =>
  esc(s)
    .replace(/@[\w.]+(?:\([^()]*\))?/g, '<span class="a">$&</span>')
    .replace(
      /\(([^\s()]+:\d+:\d+(?:-\d+)?)\)/g,
      '(<span class="ref">$1</span>)',
    );

/**
 * How a report entry is drawn: its shading, its grid cell, its message
 * with the changed words marked, and whether a finding shows its analysis.
 * @typedef {{ cls?: string, style?: string, marked?: string, tagged?: boolean }} EntryStyle
 */

/**
 * One entry of a report drawn: a finding as its analysis tagged, its
 * location and its message; an elision muted; another paragraph as its
 * lines.
 * @param {ReportEntry} e
 * @param {EntryStyle} [how]
 */
function reportEntry(
  e,
  { cls = "", style = "", marked = "", tagged = true } = {},
) {
  const msg = `<div class="msg">${marked || e.message.map(e.elision ? esc : reportMessage).join("\n")}</div>`;
  const tag = tagged ? `<span class="tag">${esc(e.analysis ?? "")}</span>` : "";
  const body = e.analysis
    ? `<div class="where">${tag}<code class="loc">${esc(e.location ?? "")}</code></div>${msg}`
    : msg;
  const kind = e.analysis ? "" : e.elision ? " elide" : " note";
  return `<div class="finding${kind}${cls ? ` ${cls}` : ""}"${style ? ` style="${style}"` : ""}>${body}</div>`;
}

/**
 * A reanalyze report drawn as its entries, one a row.
 * @param {string[]} lines
 * @param {boolean} [tagged] whether each finding shows its analysis
 */
const report = (lines, tagged = true) =>
  `<div class="report">${reportEntries(lines)
    .map(e => reportEntry(e, { tagged }))
    .join("")}</div>`;

/**
 * Two notes' texts with the words each lacks from the other marked, as
 * the message of each.
 * @param {ReportEntry} b
 * @param {ReportEntry} a
 * @returns {[string, string]}
 */
function noteWords(b, a) {
  const words = (/** @type {ReportEntry} */ e) =>
    e.message
      .join("\n")
      .split(/(\s+|[()[\]:,])/)
      .filter(Boolean);
  const wb = words(b);
  const wa = words(a);
  const pairs = matchLines(wb, wa, w => w);
  const draw = (
    /** @type {string[]} */ ws,
    /** @type {Set<number>} */ kept,
    /** @type {"add" | "del"} */ mark,
  ) =>
    ws
      .map((w, n) =>
        kept.has(n) || !w.trim()
          ? reportMessage(w)
          : `<span class="${mark}">${esc(w)}</span>`,
      )
      .join("");
  return [
    draw(wb, new Set(pairs.map(p => p[0])), "del"),
    draw(wa, new Set(pairs.map(p => p[1])), "add"),
  ];
}

/**
 * Two reanalyze reports side by side, the findings aligned by identity. A
 * run of findings in both reports is one row, "N unchanged findings", that
 * opens to show them, each facing itself. A finding one report lacks has
 * its own row, facing a gap, shaded removed in Before and added in After.
 * A note that changes, such as the closing count, faces its other version
 * on one row, the words that differ marked. Elisions align like notes,
 * muted and unshaded. Between two unchanged entries, the removed findings
 * come first, then the added ones, then the elisions and the changed
 * notes, so that a narrow screen, which stacks the cells in one column and
 * hides the After copy of each unchanged entry and the gaps, reads the
 * pair as a unified diff.
 * @param {string[]} b
 * @param {string[]} a
 * @param {boolean} [tagged] whether each finding shows its analysis
 */
function reportRows(b, a, tagged = true) {
  const before = reportEntries(b);
  const after = reportEntries(a);
  const pairs = matchLines(
    before.map(e => e.key),
    after.map(e => e.key),
    k => k,
  );
  const entry = (
    /** @type {ReportEntry} */ e,
    /** @type {number} */ r,
    /** @type {1 | 2} */ c,
    cls = "",
    marked = "",
  ) => reportEntry(e, { cls, style: `--r:${r};--c:${c}`, marked, tagged });
  const gap = (/** @type {number} */ r, /** @type {1 | 2} */ c) =>
    `<div class="finding gap" style="--r:${r};--c:${c}"></div>`;
  /** @type {string[]} */
  const out = [];
  /** @type {ReportEntry[]} The current run of unchanged findings. */
  let same = [];
  let r = 1;
  const collapse = () => {
    if (!same.length) return;
    const n = same.length;
    const rows = same
      .map((e, k) => entry(e, k + 1, 1) + entry(e, k + 1, 2, "dup"))
      .join("");
    out.push(
      `<details class="same" style="--r:${r++}"><summary>${n} unchanged finding${n === 1 ? "" : "s"}</summary><div class="report rows-report">${rows}</div></details>`,
    );
    same = [];
  };
  /** The entries of both sides that `keep` selects, paired in order. */
  const pairUp = (
    /** @type {ReportEntry[]} */ dels,
    /** @type {ReportEntry[]} */ adds,
    /** @type {(e: ReportEntry) => boolean} */ keep,
  ) => {
    const d = dels.filter(keep);
    const a = adds.filter(keep);
    return d
      .slice(0, a.length)
      .map((e, k) => /** @type {[ReportEntry, ReportEntry]} */ ([e, a[k]]));
  };
  let i = 0;
  let j = 0;
  for (const [x, y] of [...pairs, [before.length, after.length]]) {
    const dels = before.slice(i, x);
    const adds = after.slice(j, y);
    if (dels.length || adds.length) collapse();
    const elisions = pairUp(dels, adds, e => !!e.elision);
    const notes = pairUp(dels, adds, e => !e.analysis && !e.elision);
    const paired = new Set([...elisions, ...notes].flat());
    for (const e of dels.filter(e => !paired.has(e)))
      out.push(entry(e, r, 1, e.elision ? "" : "del"), gap(r++, 2));
    for (const e of adds.filter(e => !paired.has(e)))
      out.push(gap(r, 1), entry(e, r++, 2, e.elision ? "" : "add"));
    for (const [eb, ea] of elisions)
      out.push(entry(eb, r, 1), entry(ea, r++, 2));
    for (const [nb, na] of notes) {
      const [mb, ma] = noteWords(nb, na);
      out.push(entry(nb, r, 1, "mod", mb), entry(na, r++, 2, "mod", ma));
    }
    if (x < before.length) {
      if (before[x].analysis) same.push(before[x]);
      else {
        collapse();
        out.push(entry(before[x], r, 1), entry(after[y], r++, 2, "dup"));
      }
    }
    i = x + 1;
    j = y + 1;
  }
  collapse();
  return `<div class="report rows-report">${out.join("")}</div>`;
}

/**
 * Prose: paragraphs separated by blank lines; lines indented by 4 spaces or
 * more past the paragraphs are a ReScript code block.
 * @param {string[]} lines
 */
function prose(lines) {
  /** @type {{ code: boolean, lines: string[] }[]} */
  const chunks = [];
  for (const l of lines) {
    const code = isIndented(l);
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
 * Each drawing's function, given the lines of one version and, for the
 * drawings a Before/After pair shows side by side as figures (rules and
 * rewrites), the parts the other version lacks.
 * @type {{ [D in Drawing]: (lines: string[], marks?: Marks) => string }}
 */
const DRAW = {
  math: (lines, marks) => rules(lines, marks),
  tree: lines => tree(lines),
  matrix: lines => matrix(lines),
  mapping: lines => mapping(lines),
  rescript: (lines, marks) => code(lines, "rescript", marks),
  js: (lines, marks) => code(lines, "js", marks),
  lambda: (lines, marks) => code(lines, "lambda", marks),
  text: lines => `<div class="text">${codeLines(lines, esc)}</div>`,
  report: lines => report(lines),
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
 * One section with the sections it holds (`parts`: an example and forms).
 * An object block's heading `<Kind> <added|modified|removed> (<object>)`
 * is drawn as its action, kind and object; any other name as written. The
 * section's own text is prose; its indented lines are prose under a name
 * and ReScript code in an unnamed section.
 * @param {Section} s
 * @param {Section[]} parts
 */
function section(s, parts) {
  const block = BLOCK.exec(s.name);
  const kind = block?.[1] ?? s.name;
  const action = block?.[2] ?? "";
  const target = block?.[3] ?? "";
  const head = s.name
    ? `<h4 class="change">${action ? `<span class="act ${action}">${action}</span>` : ""}<span class="kind">${esc(kind)}</span>${
        target ? ` <span class="target">${inline(target)}</span>` : ""
      }</h4>`
    : "";
  const text = s.text.length ? `<p>${inline(s.text.join(" "))}</p>` : "";
  const lines = dedent(s.block);
  const body = !lines.length
    ? ""
    : s.name
      ? prose(lines)
      : code(lines, "rescript");
  const cls = kind
    ? ` k-${esc(kind.toLowerCase().replace(/[^a-z0-9]+/g, "-"))}`
    : "";
  return `<section class="block${cls}">${head}${text}${body}${forms(parts)}</section>`;
}

/**
 * The sections an object block holds, in order: its `Example:` program,
 * then each form, the form's name over its drawing. A form with both
 * versions is drawn Before and After as aligned rows, or once when they are
 * equal; a form with one version (an object added or removed, or one
 * version written) is drawn across the block under its version's label.
 * @param {Section[]} parts
 */
function forms(parts) {
  /** @type {(string | { form: string, before?: Section, after?: Section })[]} */
  const out = [];
  for (const p of parts) {
    const [, which, form = ""] = PART.exec(p.name) ?? [];
    if (!which) {
      out.push(code([...p.text, ...dedent(p.block)], "rescript"));
      continue;
    }
    const side = which === "Before" ? "before" : "after";
    let entry = out.find(
      e => typeof e !== "string" && e.form === form && !e[side],
    );
    if (!entry || typeof entry === "string") {
      entry = { form };
      out.push(entry);
    }
    entry[side] = p;
  }
  return out
    .map(e => {
      if (typeof e === "string") return e;
      const drawing = FORMS[e.form] ?? "text";
      const { before, after } = e;
      // A version's text on its heading line captions the block under it,
      // or is the version itself when no block follows.
      const hasBlock = (/** @type {Section} */ s) =>
        s.block.some(l => l.trim());
      const caption = (/** @type {Section | undefined} */ s) =>
        s && hasBlock(s) && s.text.length ? inline(s.text.join(" ")) : "";
      const lines = (/** @type {Section} */ s) =>
        hasBlock(s) ? dedent(s.block) : s.text;
      // The analysis all of a report's findings name tags the form once.
      const shared =
        drawing === "report"
          ? sharedAnalysis([before, after].flatMap(s => (s ? [lines(s)] : [])))
          : "";
      const tag = `<h5 class="form"><span class="lang">${esc(e.form)}</span>${shared ? ` <span class="tag">${esc(shared)}</span>` : ""}</h5>`;
      const draw = (/** @type {string[]} */ ls) =>
        drawing === "report" ? report(ls, !shared) : DRAW[drawing](ls);
      const one = (/** @type {string[]} */ ls, /** @type {string} */ label) =>
        `<figure class="one"><figcaption class="vlabel">${label}</figcaption>${draw(ls)}</figure>`;
      const label = (
        /** @type {string} */ name,
        /** @type {string[]} */ caps,
      ) =>
        name +
        [...new Set(caps.filter(Boolean))]
          .map(c => ` <span class="cap">${c}</span>`)
          .join("");
      if (before && after) {
        const [b, a] = [lines(before), lines(after)];
        /** @type {[string, string]} */
        const caps = [caption(before), caption(after)];
        return (
          tag +
          (b.length && b.join("\n") === a.join("\n")
            ? one(a, label("Before and After are equal", caps))
            : versions(drawing, b, a, caps, !shared))
        );
      }
      const only = /** @type {Section} */ (before ?? after);
      return (
        tag +
        one(lines(only), label(before ? "Before" : "After", [caption(only)]))
      );
    })
    .join("");
}

/** Drawings whose lines are drawn one a row, so two versions align by line. */
const LINED = new Set([
  "tree",
  "rescript",
  "js",
  "lambda",
  "text",
  "matrix",
  "mapping",
]);

/**
 * Two versions of an object side by side, each in the drawing of its form,
 * the lines each lacks from the other shaded. A caption may follow the
 * version's label. A drawing of lines (a tree, code, text, a matrix, a
 * mapping) is drawn as rows: a line common to both versions faces itself,
 * and the lines between are padded so that the next common line faces
 * itself again. A reanalyze report is drawn as rows of findings, aligned
 * by identity (`reportRows`). Typing rules and rewrites are drawn as two
 * figures side by side, the changed parts shaded: premises, conclusions, rule names and
 * formulas; a rewrite's terms, each compared with the term at its position,
 * and its side conditions.
 * @param {Drawing} drawing
 * @param {string[]} b
 * @param {string[]} a
 * @param {[string, string]} [captions]
 * @param {boolean} [tagged] whether each finding of a report shows its
 *   analysis
 */
function versions(drawing, b, a, captions = ["", ""], tagged = true) {
  const head = (
    /** @type {string} */ label,
    /** @type {string} */ caption,
    tag = "figcaption",
  ) =>
    `<${tag} class="vlabel ${label === "Before" ? "del" : "add"}">${label}${caption ? ` <span class="cap">${caption}</span>` : ""}</${tag}>`;
  const isCode =
    drawing === "rescript" || drawing === "js" || drawing === "lambda";
  const rewrite = isCode && [...b, ...a].some(l => l.includes("⇝"));
  if (drawing === "report")
    return `<div class="pair">${head("Before", captions[0], "div")}${head("After", captions[1], "div")}${reportRows(b, a, tagged)}</div>`;
  if (LINED.has(drawing) && !rewrite)
    return `<div class="pair">${head("Before", captions[0], "div")}${head("After", captions[1], "div")}${pairRows(drawing, b, a)}</div>`;
  const atoms = drawing === "math" ? mathAtoms : codeAtoms;
  const [del, add] = atomDiff(atoms(b), atoms(a));
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
  const both = [...b, ...a];
  const widths = both
    .map(cellsOf)
    .reduce(
      (w, ps) =>
        Array.from({ length: Math.max(w.length, ps.length) }, (_, c) =>
          Math.max(w[c] ?? 0, ps[c]?.length ?? 0),
        ),
      /** @type {number[]} */ ([]),
    );
  const width = Math.max(0, ...both.map(l => mapsTo(l)?.[0].length ?? 0));
  /** @param {string[]} ls @returns {string[]} */
  const html = ls =>
    drawing === "text"
      ? ls.map(esc)
      : drawing === "matrix"
        ? matrixLines(ls, widths)
        : drawing === "mapping"
          ? mappingLines(ls, width)
          : codeHtml(ls, c =>
              highlight(c, /** @type {keyof typeof LANG} */ (drawing)),
            );
  /** @param {string[]} ls @returns {{ html: string, indent: number }[]} */
  const cells = ls =>
    isTree
      ? treeRows(ls)
      : html(ls).map((h, i) => ({ html: h, indent: indentOf(ls[i]) }));
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
      const elide = elided(before[row.a ?? 0]) ? " elide" : "";
      out.push(at(n + 2, 1, left[row.a ?? 0], elide.trim()));
      out.push(at(n + 2, 2, right[row.b ?? 0], `dup${elide}`));
      n++;
      continue;
    }
    let end = n;
    while (end < rows.length && !rows[end].same) end++;
    const run = rows.slice(n, end);
    // A changed line is shaded, an elision muted.
    /** @param {number | null} k @param {string[]} ls @param {string} cls */
    const mark = (k, ls, cls) =>
      k === null || !ls[k].trim() ? "" : elided(ls[k]) ? "elide" : cls;
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
 * The sections of a record in order, each with the example and forms that
 * follow it.
 * @param {Section[]} sections
 */
function drawSections(sections) {
  const out = [];
  for (let i = 0; i < sections.length; i++) {
    const s = sections[i];
    const leading = PART.test(s.name);
    const parts = leading ? [s] : [];
    while (sections[i + 1] && PART.test(sections[i + 1].name))
      parts.push(sections[++i]);
    out.push(section(leading ? { name: "", text: [], block: [] } : s, parts));
  }
  return out.join("\n");
}

// ---------------------------------------------------------------------------
// Cards

/** The sections drawn below the changes. */
const BELOW = new Set(["Breaking", "Review"]);

/**
 * A commit's card: its identity, then its message drawn.
 * @param {CommitRecord} r
 */
function card(r) {
  const pr = /\(#(\d+)\)\s*$/.exec(r.subject)?.[1] ?? "";
  const subject = r.subject.replace(/\s*\(#\d+\)\s*$/, "");
  const breaking = r.sections.filter(s => s.name === "Breaking");
  const reviews = r.sections.filter(s => s.name === "Review");
  const joined = (/** @type {Section} */ s) =>
    [...s.text, ...s.block.map(l => l.trim())].join(" ").trim();
  const out = [
    `<article class="card${breaking.length ? " breaking" : ""}" id="c-${esc(r.sha)}">`,
    `<header><div class="meta"><code>${esc(r.sha)}</code> · ${esc(r.author)}${r.date ? ` · ${esc(r.date)}` : ""}${
      pr ? ` · <a href="${REPO_URL}/pull/${pr}">#${pr}</a>` : ""
    }</div><h3>${inline(subject)}</h3></header>`,
  ];
  out.push(drawSections(r.sections.filter(s => !BELOW.has(s.name))));

  const below = breaking.map(
    s =>
      `<div class="banner"><strong>Breaking</strong> ${inline(joined(s))}</div>`,
  );
  if (reviews.length)
    below.push(
      `<h4>Review</h4><ul class="review">${reviews
        .map(s => `<li>${inline(joined(s))}</li>`)
        .join("")}</ul>`,
    );
  if (below.length) out.push(`<div class="below">${below.join("\n")}</div>`);
  if (r.coauthors.length)
    out.push(`<div class="meta">with ${esc(r.coauthors.join(" · "))}</div>`);
  out.push("</article>");
  return out.join("\n");
}

// ---------------------------------------------------------------------------
// The page

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
 * title, when given), then each range's cards.
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
  return `<main><header>${h1}<p class="lede">${commits} commit${commits === 1 ? "" : "s"}, drawn from ${commits === 1 ? "its message" : "their messages"}.</p></header>
${groups
  .map(
    g =>
      (g.range ? `<h2><code>${esc(g.range)}</code></h2>` : "") +
      g.records.map(card).join("\n"),
  )
  .join("\n")}
</main>`;
}

// ---------------------------------------------------------------------------
// Styles: one sheet for the page, light and dark

const LIGHT = `--bg:#f5f4f1; --surface:#fff; --ink:#1c1b1a; --muted:#68625c; --rule:#dfdad4; --accent:#c8373c;
  --code-bg:#f1eee9; --math-bg:#f6f2ea; --k:#9b2a8a; --s:#277a3e; --n:#ad5a0c; --c:#8b857f; --t:#22609f; --a:#86600a; --p:#6f43b0;
  --add:#e1f2e4; --del:#f9e3e1; --ok:#25764a; --ok-bg:#e1f1e6; --warn:#955a05; --warn-bg:#fbefd8; --why:#7a560f; --why-bg:#faf3e3;`;
const DARK = `--bg:#141312; --surface:#1d1b1a; --ink:#ebe7e3; --muted:#a29b95; --rule:#36312e; --accent:#f2777b;
  --code-bg:#24211f; --math-bg:#25221d; --k:#e08fd2; --s:#91d3a2; --n:#efb26e; --c:#8b857f; --t:#8db9ee; --a:#ddbd6d; --p:#c4a4ef;
  --add:#1b3223; --del:#3a1f1d; --ok:#8fd3a2; --ok-bg:#1b3223; --warn:#efc06d; --warn-bg:#3a2d16; --why:#ddbd6d; --why-bg:#2b2416; color-scheme:dark;`;

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
p { margin:2px 0; max-width:75ch } .lede { color:var(--muted) }
a { color:var(--accent) } :focus-visible { outline:2px solid var(--accent); outline-offset:2px }
code { font:12.5px/1.5 var(--mono); background:var(--code-bg); padding:1px 4px; border-radius:3px; overflow-wrap:anywhere }
.card { background:var(--surface); border:1px solid var(--rule); border-radius:10px; padding:20px 22px; display:flex; flex-direction:column; gap:4px; min-width:0 }
.card.breaking { border-top:4px solid var(--accent) }
.meta { font:12px/1.5 var(--mono); color:var(--muted) } .meta code { background:none; padding:0 }
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
.ln.add, .cond.add { background:var(--add) } .ln.del, .cond.del { background:var(--del) }
.k { color:var(--k) } .s { color:var(--s) } .n { color:var(--n) } .c { color:var(--c); font-style:italic } .t { color:var(--t) } .a { color:var(--a) } .p { color:var(--p) } .v { color:var(--t); font-style:italic }
.paren, .stamp, .branch, .tree .label { color:var(--c) } .stamp { font-size:.85em }
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
h5.form { margin:12px 0 4px } figure.one { margin:0 }
.lang { font:500 10px/1 var(--mono); border:1px solid var(--rule); border-radius:4px; padding:2px 5px; color:var(--muted) }
.text pre { background:var(--surface); border:1px solid var(--rule); border-left:3px solid var(--accent) } .text pre .ln { white-space:pre-wrap }
.rules { display:flex; flex-direction:column; gap:14px; background:var(--math-bg); border-left:3px solid var(--accent); border-radius:6px; padding:14px 18px; overflow-x:auto; font:16px/1.6 var(--math) }
.rules .add { background:var(--add) } .rules .del { background:var(--del) }
.rules sub, .rules sup { font-size:.72em; line-height:0 }
.formulas { display:flex; flex-direction:column; gap:4px } .formulas .row { display:flex; flex-wrap:wrap; gap:4px 28px }
.infer { display:flex; align-items:center; gap:12px } .frac { display:inline-flex; flex-direction:column; align-items:center; min-width:0 }
.premises { display:flex; flex-wrap:wrap; justify-content:center; gap:4px 26px; padding:0 6px 3px } .bar { align-self:stretch; border-top:1.5px solid var(--ink) }
.conclusion { padding:3px 6px 0; text-align:center } .rule-name { font:15px/1 var(--math); font-variant:small-caps; color:var(--accent); white-space:nowrap }
var { font-style:italic }
code.m-code { font:13px/1.5 var(--mono); background:var(--surface); border:1px solid var(--rule); padding:1px 5px; border-radius:4px }
.rewrites { display:flex; flex-direction:column; gap:8px }
.rewrite { display:grid; grid-template-columns:minmax(0,1fr) repeat(var(--n, 1), auto minmax(0,1fr)); align-items:center; gap:10px } .rewrite pre { height:100% }
.to { color:var(--accent); font:600 20px/1 var(--math) }
.cond { display:flex; gap:10px; align-items:baseline; padding-left:1em; font:15px/1.5 var(--math) }
.cond .label { font:600 11px/1 var(--body); letter-spacing:.05em; text-transform:uppercase; color:var(--muted); min-width:5em }
.matrix { display:grid; gap:2px 18px; background:var(--code-bg); border-radius:6px; padding:10px 14px; overflow-x:auto; justify-content:start }
.matrix .cell { background:none; padding:0 4px; white-space:pre }
.mapping { display:flex; flex-direction:column; gap:4px; background:var(--math-bg); border-left:3px solid var(--accent); border-radius:6px; padding:10px 14px }
.map { display:flex; flex-wrap:wrap; gap:6px 12px; align-items:baseline } .map code { background:var(--surface); border:1px solid var(--rule) }
.below { display:flex; flex-direction:column; gap:6px; margin-top:12px; padding-top:10px; border-top:1px solid var(--rule) }
.banner { background:var(--del); border-left:4px solid var(--accent); padding:8px 14px; border-radius:6px } .banner strong { color:var(--accent); margin-right:6px; text-transform:uppercase; font-size:12px; letter-spacing:.06em }
ul.review { margin:0; padding-left:18px } ul.review li { margin:2px 0 }
.report { display:flex; flex-direction:column; gap:4px; margin:4px 0 }
.finding { background:var(--surface); border:1px solid var(--rule); border-radius:6px; padding:6px 12px; min-width:0 }
.finding.note { color:var(--muted) }
.finding.add { background:var(--add); border-left:3px solid var(--ok) } .finding.del { background:var(--del); border-left:3px solid var(--accent) }
.finding.mod .add { background:var(--add); color:var(--ok); border-radius:3px } .finding.mod .del { background:var(--del); color:var(--accent); border-radius:3px }
.finding.gap { border:1px dashed var(--rule); background:repeating-linear-gradient(135deg, var(--code-bg) 0 5px, transparent 5px 8px) }
.where { display:flex; flex-wrap:wrap; align-items:center; gap:4px 10px; margin-bottom:2px }
.tag { font:600 10.5px/1 var(--body); letter-spacing:.04em; padding:4px 7px; border-radius:4px; color:var(--warn); background:var(--warn-bg) }
.loc { background:none; padding:0; color:var(--t) }
.msg { white-space:pre-wrap; overflow-wrap:anywhere; font-size:14px } .note .msg { font:12.5px/1.6 var(--mono) }
.msg .ref { font:12px/1.5 var(--mono); color:var(--muted) }
.rows-report { grid-column:1 / -1; display:grid; grid-template-columns:minmax(0,1fr) minmax(0,1fr); gap:6px 10px }
.rows-report > .finding { grid-row:var(--r); grid-column:var(--c) }
.finding.elide { color:var(--muted); background:none; border-style:dashed } .ln.elide, .ln.elide * { color:var(--muted) }
.rows-report > .same { grid-row:var(--r); grid-column:1 / -1 }
.same > summary { cursor:pointer; color:var(--muted); font-size:12.5px; padding:4px 12px; border:1px dashed var(--rule); border-radius:6px }
.same[open] > summary { margin-bottom:6px }
pre, code, .rows { font-variant-ligatures:none }
@media (max-width:720px) { .versions, .rewrite, .rows { grid-template-columns:minmax(0,1fr) }
  .pair { display:block } .pair > .vlabel { display:inline-block; margin-right:14px } .rows > span { grid-row:auto; grid-column:1 } .rows > .dup, .rows > .gap { display:none }
  .rows-report { grid-template-columns:minmax(0,1fr) } .rows-report > .finding, .rows-report > .same { grid-row:auto; grid-column:1 } .rows-report > .dup, .rows-report > .gap { display:none } .rewrite .to { justify-self:center } .card { padding:16px } }`;
