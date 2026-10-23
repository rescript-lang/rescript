import assert from "node:assert/strict";
import test from "node:test";

import { commentMarkdown, fence, inline, texMath } from "./recordMarkdown.js";
import { parseRecord } from "./recordRender.js";

const SIGNED = "Signed-off-by: A Person <a@example.com>";

/**
 * The comment drawn from commit messages, one commit each.
 * @param {string[]} messages
 * @param {Parameters<typeof commentMarkdown>[1]} [options]
 */
const comment = (messages, options) =>
  commentMarkdown(
    [
      {
        records: messages.map((message, k) =>
          parseRecord({
            sha: `${k}`.padStart(9, "a"),
            author: "A Person",
            date: "Oct 22, 2026",
            message,
          }),
        ),
      },
    ],
    options,
  );

/**
 * Asserts that the comment contains the text; a failure names the text.
 * @param {string} md
 * @param {string} text
 */
const has = (md, text) =>
  assert.ok(md.includes(text), `comment lacks ${JSON.stringify(text)}`);

test("a commit is a block with its identity, sections and co-authors", () => {
  const md = comment(
    [
      `Represent raw extensions as their own node (#12)

Why:
    A raw extension carries JavaScript.

AST modified (raw extensions):
Example:
    let f = %raw(\`(a) => a\`)
Before (parse):
    [
      expression
        Pexp_extension "raw"
    ]
After (parse):
    [
      expression
        Pexp_raw "raw"
    ]

Review: Does a PPX still see it? Yes.
Breaking: PPXs must match Pexp_raw.

Co-Authored-By: B Person <b@example.com>
${SIGNED}
`,
    ],
    {
      url: "https://cdn.example/pr-12/index.html?v=1",
      head: "aaaaaaaa0",
      updated: "Oct 23, 2026, 11:48 UTC",
    },
  );
  has(
    md,
    "### [Semantic Diff](https://cdn.example/pr-12/index.html?v=1)\n\n1 commit · aaaaaaaa0 · Oct 23, 2026, 11:48 UTC · [Full page](https://cdn.example/pr-12/index.html?v=1)\n\n<details",
  );
  has(
    md,
    "<details open><summary><b>Represent raw extensions as their own node</b></summary>",
  );
  has(md, "<sub>aaaaaaaa0 · A Person + B Person · Oct 22, 2026 · #12</sub>");
  has(md, "> **Why**\n>\n> A raw extension carries JavaScript.");
  has(md, "**AST modified** (raw extensions)");
  has(md, "```rescript\nlet f = %raw(`(a) => a`)\n```");
  // the tree's list brackets are dropped and the changed line is a diff
  has(
    md,
    '`parse`\n\n```diff\n   expression\n-    Pexp_extension "raw"\n+    Pexp_raw "raw"\n```',
  );
  has(md, "> ⚠️ **Breaking:** PPXs must match Pexp\\_raw.");
  assert.ok(!md.includes("[!"));
  has(md, "**Review**\n\n- Does a PPX still see it? Yes.");
  has(md, "Yes.\n\n</details>");
  assert.ok(!md.includes("Signed-off-by"));
});

test("a typing rule is mathematics, the changed parts coloured", () => {
  const md = comment([
    `Check applications

Typing modified (application):
Before (rule):
    Γ ⊢ e_1 : τ_{1} → τ_{2}     Γ ⊢ e_2 : τ_{1}
    ─────────────── (App)
    Γ ⊢ e_1 e_2 : τ_{2}
After (rule):
    Γ ⊢ e_1 : τ_{1} → τ_{2}     Γ ⊢ e_2 : τ_{1}′
    ─────────────── (App)
    Γ ⊢ e_1 e_2 : τ_{2}

${SIGNED}
`,
  ]);
  has(md, "<table><tr><th>Before</th><th>After</th></tr><tr><td>\n\n```math\n");
  has(
    md,
    "\\dfrac{Γ⊢e_{1}:τ_{1}→τ_{2} \\qquad \\color{red}{Γ⊢e_{2}:τ_{1}}}{\\begin{array}{c}Γ⊢e_{1}\\ e_{2}:τ_{2}\\end{array}} \\quad (\\textsf{App})",
  );
  has(md, "\\color{green}{Γ⊢e_{2}:τ_{1}'}");
  assert.equal(md.split("\\color").length - 1, 2);
});

test("a formula's words, code and TeX characters", () => {
  assert.equal(
    texMath("fresh x   `f(a, ...)` & {p}"),
    "\\mathrm{fresh}\\ x\\texttt{f(a, ...)}\\&\\{p\\}",
  );
});

test("matrices and mappings align their columns in the diff", () => {
  const md = comment([
    `Unbox options

Representation modified (option):
Before (representation): boxed
    Some(x) ↦ {_0: x}
    None ↦ undefined
After (representation): unboxed
    Some(x) ↦ x
    None ↦ undefined

Matching modified (option split):
Before (matrix):
    Some(x)   e1
    None   e2
After (matrix):
    Some(x)   _   e1
    None   e2
    order: Some first

JS modified (f):
Before (js): same output
    let f = 1;
After (js): same output
    let f = 1;

${SIGNED}
`,
  ]);
  has(
    md,
    "`representation` · Before: boxed · After: unboxed\n\n```diff\n-Some(x) ↦ {_0: x}\n+Some(x) ↦ x\n None    ↦ undefined\n```",
  );
  has(
    md,
    "```diff\n-Some(x)   e1\n+Some(x)   _    e1\n None      e2\n+order: Some first\n```",
  );
  has(
    md,
    "`js` · Before and After are equal · same output\n\n```js\nlet f = 1;\n```",
  );
});

const DIVIDE = `Exception Analysis
Division.res:1:5-10
divide might throw Division_by_zero`;
const DIVIDE_BY = DIVIDE.replace(/divide/g, "divideBy").replace(":1:", ":2:");

test("a reanalyze report is a diff of findings, unchanged ones counted", () => {
  const indented = (/** @type {string} */ s) =>
    s
      .split("\n")
      .map(l => (l ? `    ${l}` : ""))
      .join("\n");
  const md = comment([
    `Skip literal divisors

Reanalyze modified (division):
Before (reanalyze):
${indented(`${DIVIDE}\n\n${DIVIDE_BY}\n\n${DIVIDE_BY.replaceAll("divideBy", "divideTo")}\n\nAnalysis reported 3 issues`)}
After (reanalyze):
${indented(`${DIVIDE_BY}\n\n${DIVIDE_BY.replaceAll("divideBy", "divideTo")}\n\nAnalysis reported 2 issues`)}

${SIGNED}
`,
  ]);
  has(
    md,
    "```diff\n-Exception Analysis\n-Division.res:1:5-10\n-divide might throw Division_by_zero\n \n … 2 unchanged findings\n \n-Analysis reported 3 issues\n \n+Analysis reported 2 issues\n```",
  );
});

test("prose escapes markup and draws attributes and mentions as code", () => {
  assert.equal(
    inline("Use @react.component and `a*b` for <T> in $x_1 * 2, cc @someone"),
    "Use `@react.component` and `a*b` for \\<T\\> in \\$x\\_1 \\* 2, cc `@someone`",
  );
  assert.equal(inline("# not a heading"), "\\# not a heading");
  assert.equal(inline("1. not a list"), "1\\. not a list");
  assert.equal(inline("thanks @0x, @9"), "thanks `@0x`, `@9`");
  assert.equal(inline("mail a@example.com"), "mail a@example.com");
});

test("a fence is longer than any run of backticks in its lines", () => {
  assert.equal(fence("js", ["let s = ```;"]), "````js\nlet s = ```;\n````");
});

test("past the budget, the largest commits leave their forms to the page", () => {
  const big = `Change output

JS modified (f):
Before (js):
${Array.from({ length: 200 }, (_, k) => `    let a${k} = ${k};`).join("\n")}
After (js):
${Array.from({ length: 200 }, (_, k) => `    let b${k} = ${k};`).join("\n")}

${SIGNED}
`;
  const small = `Fix a typo\n\nWhy:\n    It was wrong.\n\n${SIGNED}\n`;
  const md = comment([small, big, small], { budget: 4000 });
  assert.ok(md.length <= 4000, `${md.length}`);
  has(md, "**JS modified** (f)\n\n<sub>Forms on the page.</sub>");
  assert.equal(md.split("> It was wrong.").length - 1, 2);
  const tiny = comment([small, big, small], { budget: 400 });
  assert.ok(tiny.length <= 500, `${tiny.length}`);
  has(tiny, "more commit");
});

test("an assistant co-author is drawn by its short name, others as written", () => {
  const md = comment([
    `Fix it\n\nCo-Authored-By: Claude Sonnet 5.5 <NoReply@Anthropic.com>\nCo-Authored-By: Claude Person <claude@example.com>\nCo-Authored-By: No Email\n${SIGNED}\n`,
  ]);
  has(md, "· A Person + Claude + Claude Person + No Email ·");
});
