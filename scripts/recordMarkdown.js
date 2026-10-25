// @ts-check

/* Commit records drawn as GitHub Markdown, for the Semantic Diff comment on
 * a pull request. GitHub strips styles from a comment, so each form is drawn
 * in what its Markdown renders natively: lines of code, trees, text,
 * matrices, mappings and reanalyze reports as a unified diff, typing rules
 * as mathematics. A comment holds at most 65536 characters; past a budget
 * below that, the largest commits are drawn without their forms, and the
 * page has them all. recordRender.js parses the records and draws the page.
 */

import {
  alignLines,
  atomDiff,
  BELOW,
  BLOCK,
  byline,
  cellsOf,
  columnWidths,
  dedent,
  esc,
  FORMS,
  formEntries,
  mappingWidth,
  mapsTo,
  matchLines,
  mathAtoms,
  mathGroups,
  mathTokens,
  NOTE,
  proseChunks,
  reportEntries,
  sectionGroups,
  treeLine,
} from "./recordRender.js";

/** @import { CommitRecord, Group, Section, FormEntry } from "./recordRender.js" */

/**
 * A changelog fragment the pull request adds: its category, the category's
 * heading as the changelog has it (`#### :boom: Breaking Change`), and its
 * Markdown bullets.
 * @typedef {{ category: string, heading: string, text: string }} ChangelogEntry
 */

/** The characters a comment holds at most, with room for the action's tag. */
export const BUDGET = 60000;

// ---------------------------------------------------------------------------
// Inline text

/**
 * Prose split into its code spans and attributes (odd indices) and the
 * text between (even ones). An attribute such as `@react.component` written
 * outside backticks is drawn as code, so that GitHub does not take it for a
 * mention of a user; so is any other `@name`.
 * @param {string} s
 */
const spans = s =>
  s.split(/(`[^`]+`|(?<![\w@`])@{1,2}\w[\w.]*(?:\([^()`\s]*\))?)/);

/**
 * Markdown kept as written, but for an `@name` outside backticks, drawn as
 * code so that it notifies no one.
 * @param {string} s
 */
export const unmentioned = s =>
  spans(s)
    .map((part, i) => (i % 2 && !part.startsWith("`") ? `\`${part}\`` : part))
    .join("");

/**
 * Prose as Markdown: code spans kept, attributes drawn as code, the
 * characters Markdown or GitHub would read as markup escaped. `#1234` and
 * shas stay as written, which GitHub links.
 * @param {string} s
 */
export const inline = s =>
  spans(s)
    .map((part, i) =>
      i % 2
        ? part.startsWith("`")
          ? part
          : `\`${part}\``
        : part.replace(/&/g, "&amp;").replace(/[\\`*_[\]<>|~$]/g, "\\$&"),
    )
    .join("")
    .replace(/^[#+-]/, "\\$&")
    .replace(/^(\d+)([.)])/, "$1\\$2");

/**
 * Prose as HTML, for a `<summary>`, where GitHub reads no Markdown: code
 * spans and attributes as `<code>`.
 * @param {string} s
 */
const inlineHtml = s =>
  spans(s)
    .map((part, i) =>
      i % 2 ? `<code>${esc(part.replace(/^`|`$/g, ""))}</code>` : esc(part),
    )
    .join("");

/**
 * A fenced code block: the fence longer than any run of backticks in the
 * lines, so that no line closes it.
 * @param {string} lang
 * @param {string[]} lines
 */
export function fence(lang, lines) {
  const longest = Math.max(
    0,
    ...lines.flatMap(l => (l.match(/`+/g) ?? []).map(r => r.length)),
  );
  const f = "`".repeat(Math.max(3, longest + 1));
  return `${f}${lang}\n${lines.join("\n")}\n${f}`;
}

/**
 * Prose: paragraphs, and ReScript code blocks (`proseChunks`).
 * @param {string[]} lines
 */
const prose = lines =>
  proseChunks(lines).map(c =>
    c.code ? fence("rescript", c.lines) : inline(c.lines[0]),
  );

// ---------------------------------------------------------------------------
// Mathematics

/** Characters TeX reads as markup, escaped. @param {string} s */
const tex = s =>
  s.replace(/[#$%&_{}\\^~]/g, c =>
    c === "\\"
      ? "\\backslash "
      : c === "^"
        ? "\\hat{}"
        : c === "~"
          ? "\\sim "
          : `\\${c}`,
  );

/**
 * A formula in TeX, as the page sets it: one-letter names, with their
 * primes, italic variables, longer names upright; `_x`, `_{…}` and `^{…}`
 * sub- and superscripts; `code` in backticks as typewriter text; primes as
 * `'`. A space between two words (a name, code or a subscripted name) is
 * kept.
 * @param {string} text
 * @returns {string}
 */
export function texMath(text) {
  const toks = [...mathTokens(text)];
  const word = (/** @type {RegExpMatchArray | undefined} */ m) =>
    !!(m?.groups?.name || m?.groups?.code || m?.groups?.script);
  return toks
    .map((m, k) => {
      const g = m.groups ?? {};
      if (g.code) return `\\texttt{${tex(g.code.slice(1, -1))}}`;
      if (g.script)
        return `${g.script[0]}{${texMath(g.script.replace(/^[_^]\{?|\}$/g, ""))}}`;
      if (g.name)
        return /^.[0-9'′]*$/.test(g.name)
          ? g.name.replace(/′/g, "'")
          : `\\mathrm{${g.name.replace(/′/g, "'")}}`;
      if (/\s/.test(m[0]))
        return word(toks[k - 1]) && word(toks[k + 1]) ? "\\ " : "";
      return m[0] === "′" ? "'" : tex(m[0]);
    })
    .join("");
}

/**
 * Mathematics in TeX (`mathGroups`): inference rules as fractions, their
 * names after them, and rows of formulas; the marked parts (numbered as
 * `mathAtoms` lists them) coloured.
 * @param {string[]} block
 * @param {{ marked?: Set<number>, color?: string }} [marks]
 */
export function texRules(block, marks = {}) {
  let n = 0;
  const part = (/** @type {string} */ t) =>
    marks.marked?.has(n++) ? `\\color{${marks.color}}{${t}}` : t;
  const out = mathGroups(block).map(g => {
    if ("rows" in g)
      return `\\begin{array}{l}${g.rows
        .map(r => r.map(p => part(texMath(p))).join(" \\qquad "))
        .join(" \\\\ ")}\\end{array}`;
    const premises = g.premises.map(p => part(texMath(p))).join(" \\qquad ");
    const conclusion = g.conclusion.map(l => part(texMath(l))).join(" \\\\ ");
    const name = g.name ? ` \\quad ${part(`(\\textsf{${tex(g.name)}})`)}` : "";
    return `\\dfrac{${premises}}{\\begin{array}{c}${conclusion}\\end{array}}${name}`;
  });
  return out.length > 1
    ? `\\begin{array}{l}${out.join(" \\\\[1em] ")}\\end{array}`
    : (out[0] ?? "");
}

// ---------------------------------------------------------------------------
// Forms

/**
 * Each drawing's fenced block language for one version: GitHub highlights
 * ReScript and JavaScript; trees, Lambda, text and reports are plain.
 * @type {Record<string, string>}
 */
const LANG = { rescript: "rescript", js: "js", math: "math" };

/**
 * The lines of one version of a form as drawn in a fenced block: a tree's
 * list brackets dropped, a matrix's cells and a mapping's arrows aligned
 * across both versions (`both`).
 * @param {string} drawing
 * @param {string[]} ls
 * @param {string[]} both
 */
function formLines(drawing, ls, both) {
  if (drawing === "tree") return ls.filter(treeLine);
  if (drawing === "matrix") {
    const widths = columnWidths(both);
    return ls.map(l =>
      NOTE.test(l)
        ? l.trim()
        : cellsOf(l)
            .map((p, c) => p.padEnd(widths[c] ?? 0))
            .join("   ")
            .trimEnd(),
    );
  }
  if (drawing === "mapping") {
    const width = mappingWidth(both);
    return ls.map(l => {
      const m = mapsTo(l);
      return m ? `${m[0].padEnd(width)} ↦ ${m[1]}` : l.trim();
    });
  }
  return ls;
}

/**
 * Two versions of a form's lines as a unified diff: a line common to both
 * is context; between two common lines, the lines only Before has, then the
 * lines only After has.
 * @param {string[]} b
 * @param {string[]} a
 */
function diffLines(b, a) {
  /** @type {string[]} */
  const out = [];
  /** @type {string[]} */
  let adds = [];
  for (const row of alignLines(b, a)) {
    if (row.same) {
      out.push(...adds);
      adds = [];
      out.push(` ${b[row.a ?? 0]}`);
      continue;
    }
    if (row.a !== null) out.push(`-${b[row.a]}`);
    if (row.b !== null) adds.push(`+${a[row.b]}`);
  }
  return [...out, ...adds];
}

/**
 * Two reanalyze reports as a unified diff of their entries, aligned by
 * identity: a finding one report lacks is removed or added, a run of
 * findings in both is one context line counting them, and a note in both,
 * such as the closing count, is context.
 * @param {string[]} b
 * @param {string[]} a
 */
function reportDiff(b, a) {
  const before = reportEntries(b);
  const after = reportEntries(a);
  const pairs = matchLines(
    before.map(e => e.key),
    after.map(e => e.key),
    k => k,
  );
  /** @type {string[][]} */
  const out = [];
  let same = 0;
  const collapse = () => {
    if (same)
      out.push([` … ${same} unchanged finding${same === 1 ? "" : "s"}`]);
    same = 0;
  };
  const lines = (/** @type {string} */ key, /** @type {string} */ sign) =>
    key.split("\n").map(l => sign + l);
  let i = 0;
  let j = 0;
  for (const [x, y] of [...pairs, [before.length, after.length]]) {
    const dels = before.slice(i, x);
    const adds = after.slice(j, y);
    if (dels.length || adds.length) collapse();
    for (const e of dels) out.push(lines(e.key, e.elision ? " " : "-"));
    for (const e of adds) out.push(lines(e.key, e.elision ? " " : "+"));
    if (x < before.length) {
      if (before[x].analysis) same++;
      else {
        collapse();
        out.push(lines(before[x].key, " "));
      }
    }
    i = x + 1;
    j = y + 1;
  }
  collapse();
  return out.flatMap((e, k) => (k ? [" ", ...e] : e));
}

/**
 * One form of an object block: its name and captions, then the versions
 * drawn. Two versions are a diff, or, for typing rules, two figures side by
 * side with the changed parts coloured; equal versions and a lone version
 * are drawn once.
 * @param {Extract<FormEntry, { form: string }>} e
 */
function form(e) {
  const drawing = FORMS[e.form] ?? "text";
  const { before, after } = e;
  const hasBlock = (/** @type {Section} */ s) => s.block.some(l => l.trim());
  const caption = (/** @type {Section | undefined} */ s) =>
    s && hasBlock(s) && s.text.length ? inline(s.text.join(" ")) : "";
  const lines = (/** @type {Section} */ s) =>
    hasBlock(s) ? dedent(s.block) : s.text;
  const label = (/** @type {string[]} */ notes) =>
    [`\`${e.form}\``, ...notes.filter(Boolean)].join(" · ");
  const one = (/** @type {string[]} */ ls) =>
    drawing === "math"
      ? fence("math", [texRules(ls)])
      : fence(LANG[drawing] ?? "", formLines(drawing, ls, ls));
  if (before && after) {
    const [b, a] = [lines(before), lines(after)];
    const [cb, ca] = [caption(before), caption(after)];
    if (b.length && b.join("\n") === a.join("\n"))
      return `${label(["Before and After are equal", cb, ca === cb ? "" : ca])}\n\n${one(a)}`;
    const head = label(
      cb === ca ? [cb] : [cb && `Before: ${cb}`, ca && `After: ${ca}`],
    );
    if (drawing === "math") {
      const [del, add] = atomDiff(mathAtoms(b), mathAtoms(a));
      const cell = (/** @type {string} */ t) => `<td>\n\n${t}\n\n</td>`;
      return `${head}\n\n<table><tr><th>Before</th><th>After</th></tr><tr>${cell(
        fence("math", [texRules(b, { marked: del, color: "red" })]),
      )}${cell(fence("math", [texRules(a, { marked: add, color: "green" })]))}</tr></table>`;
    }
    const diff =
      drawing === "report"
        ? reportDiff(b, a)
        : diffLines(
            formLines(drawing, b, [...b, ...a]),
            formLines(drawing, a, [...b, ...a]),
          );
    return `${head}\n\n${fence("diff", diff)}`;
  }
  const only = /** @type {Section} */ (before ?? after);
  return `${label([before ? "Before" : "After", caption(only)])}\n\n${one(lines(only))}`;
}

// ---------------------------------------------------------------------------
// Commits

/**
 * One section with the example and forms it holds; without `full`, the
 * forms are left to the page.
 * @param {Section} s
 * @param {Section[]} parts
 * @param {boolean} full
 */
function section(s, parts, full) {
  /** @type {string[]} */
  const out = [];
  const block = BLOCK.exec(s.name);
  const text = s.text.length ? [inline(s.text.join(" "))] : [];
  const lines = dedent(s.block);
  const body = !lines.length
    ? []
    : s.name
      ? prose(lines)
      : [fence("rescript", lines)];
  if (s.name === "Why")
    out.push(
      [`**Why**`, ...text, ...body]
        .join("\n\n")
        .split("\n")
        .map(l => (l ? `> ${l}` : ">"))
        .join("\n"),
    );
  else {
    if (s.name)
      out.push(
        block
          ? `**${block[1]} ${block[2]}**${block[3] ? ` (${inline(block[3])})` : ""}`
          : `**${inline(s.name)}**`,
      );
    out.push(...text, ...body);
  }
  const entries = formEntries(parts);
  if (full)
    for (const e of entries)
      out.push("example" in e ? fence("rescript", e.example) : form(e));
  else if (entries.length) out.push("<sub>Forms on the page.</sub>");
  return out.join("\n\n");
}

/** The sign before a commit's subject that marks what the last push did. */
const SIGNS = { new: "✨", edited: "✏️" };

/**
 * A commit as a collapsible block: its subject, identity and message drawn;
 * without `full`, its forms are left to the page. A `change` (`new` or
 * `edited`), what the last push did to it, puts a sign before the subject
 * and the word in the identity line.
 * @param {CommitRecord} r
 * @param {boolean} full
 * @param {string} [change]
 */
export function commitMarkdown(r, full, change = "") {
  const pr = /\(#(\d+)\)\s*$/.exec(r.subject)?.[1] ?? "";
  const subject = r.subject.replace(/\s*\(#\d+\)\s*$/, "");
  const joined = (/** @type {Section} */ s) =>
    [...s.text, ...s.block.map(l => l.trim())].join(" ").trim();
  const out = [
    `<details open><summary>${change ? `${SIGNS[/** @type {keyof typeof SIGNS} */ (change)] ?? ""} ` : ""}<b>${inlineHtml(subject)}</b></summary>`,
    `<sub>${esc(r.sha)} · ${byline(r, inline, (login, url) => `[${login}](${url})`)}${r.date ? ` · ${esc(r.date)}` : ""}${pr ? ` · #${pr}` : ""}${change ? ` · ${esc(change)}` : ""}</sub>`,
    ...sectionGroups(r.sections.filter(s => !BELOW.has(s.name))).map(g =>
      section(g.section, g.parts, full),
    ),
    // A quote, not a `[!CAUTION]` alert: GitHub draws alerts only at the top
    // level of a comment, and inside `<details>` shows the marker as text.
    ...r.sections
      .filter(s => s.name === "Breaking")
      .map(s => `> ⚠️ **Breaking:** ${inline(joined(s))}`),
  ];
  const reviews = r.sections.filter(s => s.name === "Review");
  if (reviews.length)
    out.push(
      `**Review**\n\n${reviews.map(s => `- ${inline(joined(s))}`).join("\n")}`,
    );
  out.push("</details>");
  return out.filter(Boolean).join("\n\n");
}

/**
 * The changelog entries grouped under their categories' headings, as the
 * changelog groups them (the entries come in its order), in at most `room`
 * characters: the comment shortens commits to fit its budget but not the
 * changelog, so entries past the room are counted, not drawn.
 * @param {ChangelogEntry[]} changelog
 * @param {number} room
 */
function changelogList(changelog, room) {
  if (!changelog.length) return "No changelog entry.";
  const drawn = [];
  let size = 0;
  let heading = "";
  let count = 0;
  for (const e of changelog) {
    const lines = `${e.heading === heading ? "" : `${e.heading}\n\n`}${unmentioned(e.text)}`;
    size += lines.length + 2;
    if (size > room) break;
    drawn.push(e.heading === heading || !count ? lines : `\n${lines}`);
    heading = e.heading;
    count++;
  }
  const left = changelog.length - count;
  if (left)
    drawn.push(
      `\n<sub>${left}${count ? " more" : ""} changelog entr${left === 1 ? "y" : "ies"} under \`changes/\`.</sub>`,
    );
  return drawn.join("\n");
}

/**
 * The comment: a heading linking the page (`url`), in its title and as "Full
 * page", then each commit. Past the budget, the commits with the most
 * characters are drawn without their forms, and if that is not enough, the
 * last commits are left to the page. The commits the last push added or
 * edited (`changes`) are marked.
 *
 * The changelog entries the pull request adds (`changelog`, grouped under
 * the changelog's category headings) describe it as a whole, as its squash
 * commit will; a commit's `Breaking:` sections describe that commit. They
 * come first, under their own heading, which also notes a pull request
 * whose commits break something while no entry is a breaking change, or
 * the other way round.
 * @param {Group[]} groups
 * @param {{ url?: string, budget?: number, changes?: Map<string, string>, changelog?: ChangelogEntry[] }} [options]
 */
export function commentMarkdown(
  groups,
  { url = "", budget = BUDGET, changes = new Map(), changelog } = {},
) {
  const records = groups.flatMap(g => g.records);
  const n = records.length;
  const breakingCommits = records.filter(r =>
    r.sections.some(s => s.name === "Breaking"),
  ).length;
  const breakingEntry = changelog?.some(e => e.category === "breaking");
  const head = [
    changelog && "### Changelog",
    changelog && changelogList(changelog, budget / 4),
    changelog &&
      breakingCommits &&
      !breakingEntry &&
      `<sub>:boom: ${breakingCommits === 1 ? "A commit says" : `${breakingCommits} commits say`} what it breaks (\`Breaking:\`), but no changelog entry is a breaking change.</sub>`,
    breakingEntry &&
      !breakingCommits &&
      "<sub>:boom: A changelog entry is a breaking change, but no commit says what it breaks (`Breaking:`).</sub>",
    `### ${url ? `[Semantic Diff](${url})` : "Semantic Diff"}`,
    [`${n} commit${n === 1 ? "" : "s"}`, url && `[Full page](${url})`]
      .filter(Boolean)
      .join(" · "),
  ]
    .filter(Boolean)
    .join("\n\n");
  const change = (/** @type {CommitRecord} */ r) => changes.get(r.sha) ?? "";
  const drawn = records.map(r => commitMarkdown(r, true, change(r)));
  const size = () => drawn.reduce((t, d) => t + d.length + 2, head.length);
  const order = drawn
    .map((d, k) => ({ k, length: d.length }))
    .sort((x, y) => y.length - x.length);
  for (const { k } of order) {
    if (size() <= budget) break;
    drawn[k] = commitMarkdown(records[k], false, change(records[k]));
  }
  let left = 0;
  while (drawn.length && size() > budget) {
    drawn.pop();
    left++;
  }
  if (left)
    drawn.push(
      `<sub>${left} more commit${left === 1 ? "" : "s"} on the page.</sub>`,
    );
  return `${[head, ...drawn].join("\n\n")}\n`;
}
