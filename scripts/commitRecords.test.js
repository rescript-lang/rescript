import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

const script = fileURLToPath(new URL("./commitRecords.js", import.meta.url));
const SIGNED = "Signed-off-by: A Person <a@example.com>";

/**
 * Asserts that the page contains the text; a failure names the text.
 * @param {string} html
 * @param {string} text
 */
const has = (html, text) =>
  assert.ok(html.includes(text), `page lacks ${JSON.stringify(text)}`);

/**
 * Asserts that the page lacks the text.
 * @param {string} html
 * @param {string} text
 */
const lacks = (html, text) =>
  assert.ok(!html.includes(text), `page has ${JSON.stringify(text)}`);

/**
 * @param {string} dir
 * @param {Record<string, string>} files
 */
async function write(dir, files) {
  for (const [name, content] of Object.entries(files)) {
    await fs.mkdir(path.dirname(path.join(dir, name)), { recursive: true });
    await fs.writeFile(path.join(dir, name), content);
  }
}

/**
 * Renders the records of a temporary repository's commits.
 * @param {(git: (...args: string[]) => string, dir: string) => Promise<void>} setup
 * @param {string[]} args
 */
async function render(setup, args) {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), "commit-records-"));
  const git = (/** @type {string[]} */ ...a) =>
    execFileSync("git", a, { cwd: dir, encoding: "utf8" });
  git("init", "-q", "-b", "main");
  git("config", "user.name", "A Person");
  git("config", "user.email", "a@example.com");
  await setup(git, dir);
  const out = path.join(dir, "page.html");
  const r = spawnSync("node", [script, "--out", out, ...args], {
    cwd: dir,
    encoding: "utf8",
  });
  assert.equal(r.status, 0, r.stderr);
  return fs.readFile(out, "utf8");
}

/**
 * Commits the files with the message.
 * @param {(...args: string[]) => string} git
 * @param {string} dir
 * @param {Record<string, string>} files
 * @param {string} message
 */
async function commit(git, dir, files, message) {
  await write(dir, files);
  git("add", "-A", ".");
  git("commit", "-q", "--allow-empty", "-m", message);
}

/** @param {string} tree @param {string} indent */
const indented = (tree, indent) =>
  tree
    .trimEnd()
    .split("\n")
    .map(l => indent + l)
    .join("\n");

const BEFORE_TREE = `[
  structure_item
    Pstr_value Nonrec
    [
      <def>
        pattern
          Ppat_var "f"
        expression
          Pexp_extension "raw"
    ]
]`;
const AFTER_TREE = BEFORE_TREE.replace(
  'Pexp_extension "raw"',
  'Pexp_raw "raw"\n          Raw_template "(a) => a"',
);

const AST_RECORD = `Represent raw extensions as their own node

Why:
    A raw extension carries JavaScript; its own node gives it a type.

AST modified (raw extensions):
Example:
    let f = %raw(\`(a) => a\`)
Before (parse):
${indented(BEFORE_TREE, "    ")}
After (parse):
${indented(AFTER_TREE, "    ")}

Review: Does a PPX still see the extension? Yes: the v0 bridge maps it back,
    as https://github.com/rescript-lang/rescript/pull/8624 describes.

${SIGNED}
`;

test("a minimal record draws its identity and subject", async () => {
  const html = await render(
    async (git, dir) => {
      await commit(git, dir, { "README.md": "a\n" }, "base");
      await commit(
        git,
        dir,
        { "README.md": "b\n" },
        `Fix a typo (#7)\n\nCo-Authored-By: B Person <b@example.com>\n${SIGNED}`,
      );
    },
    ["HEAD"],
  );
  has(html, "<title>Semantic Diff</title>");
  has(html, '<h1 class="brand">Semantic <span>Diff</span></h1>');
  has(html, "<h3>Fix a typo</h3>");
  assert.match(
    html,
    /<div class="meta"><code>[0-9a-f]{9}<\/code> · A Person \+ B Person · [A-Z][a-z]{2} \d{2}, \d{4}, \d{2}:\d{2} UTC · <a href="https:\/\/github.com\/rescript-lang\/rescript\/pull\/7">#7<\/a><\/div>/,
  );
  lacks(html, '<div class="below">');
  lacks(html, "Signed-off-by");
  lacks(html, "Co-Authored-By");
});

test("an AST block's parse trees are drawn as aligned Before and After rows", async () => {
  const html = await render(
    async (git, dir) => {
      await commit(git, dir, { "a.txt": "a\n" }, "base");
      await commit(git, dir, { "compiler/ml/parsetree.ml": "x\n" }, AST_RECORD);
    },
    ["HEAD"],
  );
  has(
    html,
    '<h4 class="change"><span class="act modified">modified</span><span class="kind">AST</span> <span class="target">raw extensions</span></h4>',
  );
  has(html, '<span class="a">%raw</span>');
  has(html, '<h5 class="form"><span class="lang">parse</span></h5>');
  has(html, '<div class="rows code tree">');
  // the root stands bare; the common lines face each other;
  // the changed node is a deleted row Before and two added rows After, the
  // shorter side padded with a gap
  const nb = "\u00a0";
  has(
    html,
    '<span class="ln" style="--r:2;--c:1;--i:0"><span class="branch"></span>structure_item</span>',
  );
  has(
    html,
    `<span class="ln del" style="--r:8;--c:1;--i:12"><span class="branch">${nb.repeat(9)}└─${nb}</span><span class="t">Pexp_extension</span> <span class="s">&quot;raw&quot;</span></span>`,
  );
  has(
    html,
    `<span class="ln add" style="--r:8;--c:2;--i:12"><span class="branch">${nb.repeat(9)}├─${nb}</span><span class="t">Pexp_raw</span> <span class="s">&quot;raw&quot;</span></span>`,
  );
  has(html, '<span class="ln gap" style="--r:9;--c:1;--i:0">');
  has(html, '<span class="ln add" style="--r:9;--c:2;--i:12">');
  has(html, '<span class="ln dup" style="--r:7;--c:2;--i:9">');
  has(html, '<ul class="review"><li>Does a PPX still see the extension?');
  has(
    html,
    'as <a href="https://github.com/rescript-lang/rescript/pull/8624">#8624</a> describes',
  );
  has(html, '<section class="block k-why">');
  // the example and its forms belong to the object's block
  assert.equal(html.split('<section class="block').length - 1, 2);
});

test("an added object's single form is drawn across the block", async () => {
  const html = await render(
    async (git, dir) => {
      await commit(git, dir, { "a.txt": "a\n" }, "base");
      await commit(
        git,
        dir,
        {},
        `Add a raw node\n\nAST added (Pexp_raw):\nAfter (parse):\n${indented(AFTER_TREE, "    ")}\n\n${SIGNED}\n`,
      );
    },
    ["HEAD"],
  );
  has(html, '<span class="act added">added</span>');
  has(
    html,
    '<figure class="one"><figcaption class="vlabel">After</figcaption>',
  );
  has(html, '<span class="t">Pexp_raw</span>');
  lacks(html, '<div class="rows');
});

test("a range draws each commit's forms: JS as code, a diagnostic as text", async () => {
  const html = await render(
    async (git, dir) => {
      await commit(git, dir, { "a.txt": "a\n" }, "base");
      await commit(git, dir, {}, AST_RECORD);
      await commit(
        git,
        dir,
        {},
        `Compile raw extensions directly

JS modified (raw extension):
Example:
    let f = 1
Before (js):
    let f = 1;
After (js):
    let f = 2;

Diagnostic added (unused raw):
After (diagnostic):
    Warning number 109
    This raw extension is unused.

${SIGNED}
`,
      );
    },
    ["HEAD~2..HEAD"],
  );
  has(html, "<h2><code>HEAD~2..HEAD</code></h2>");
  has(html, '<span class="lang">js</span>');
  has(
    html,
    '<span class="ln del" style="--r:2;--c:1;--i:0"><span class="k">let</span> f = <span class="n">1</span>;</span>',
  );
  has(
    html,
    '<span class="ln add" style="--r:2;--c:2;--i:0"><span class="k">let</span> f = <span class="n">2</span>;</span>',
  );
  has(html, '<span class="lang">diagnostic</span>');
  has(html, '<div class="text">');
  has(html, "This raw extension is unused.");
  assert.equal(html.split('<div class="rows code tree">').length - 1, 1);
});

test("equal Before and After forms are drawn once", async () => {
  const html = await render(
    async (git, dir) => {
      await commit(git, dir, { "a.txt": "a\n" }, "base");
      await commit(
        git,
        dir,
        {},
        `Keep the output\n\nJS modified (x):\nBefore (js):\n    let x = 1;\nAfter (js):\n    let x = 1;\n\n${SIGNED}\n`,
      );
    },
    ["HEAD"],
  );
  has(html, "Before and After are equal");
  lacks(html, '<div class="rows');
});

test("a card draws the message alone, whatever the diff", async () => {
  const message = `Change things\n\nWhy:\n    A reason.\n\n${SIGNED}\n`;
  /** @param {Record<string, string>} files */
  const card = async files => {
    const html = await render(
      async (git, dir) => {
        await commit(git, dir, { "a.txt": "a\n" }, "base");
        await commit(git, dir, files, message);
      },
      ["HEAD"],
    );
    // the card with its short hash, which differs between the two commits,
    // blanked
    const sha = /id="c-([0-9a-f]+)"/.exec(html)?.[1] ?? "";
    return html
      .slice(html.indexOf("<article"), html.indexOf("</article>"))
      .replaceAll(sha, "SHA");
  };
  const touched = await card({
    "tests/tests/src/a_test.res": "let y = 1\n",
    "tests/tests/src/a_test.mjs": "y\n",
    "tests/build_tests/super_errors/expected/x.res.expected": "e\n",
    "changes/x.fix.md": "- Fix a thing.\n",
    "compiler/ext/config.ml": 'let cmi_magic_number = "BS00MI002"\n',
  });
  assert.equal(touched, await card({}));
  has(touched, "A reason.");
  lacks(touched, "a_test");
});

test("--pr and --title head the page, escaped", async () => {
  const html = await render(
    async (git, dir) => {
      await commit(git, dir, { "a.txt": "a\n" }, `Base\n\n${SIGNED}`);
    },
    ["--pr", "42", "--title", "Fix <output> & more", "HEAD"],
  );
  has(
    html,
    '<h1><a href="https://github.com/rescript-lang/rescript/pull/42">#42</a> Fix &lt;output&gt; &amp; more</h1>',
  );
  has(html, "<title>#42 Fix &lt;output&gt; &amp; more · Semantic Diff</title>");
});

test("a typing rule draws subscripts, and a matrix its cells and notes", async () => {
  const html = await render(
    async (git, dir) => {
      await commit(git, dir, { "a.txt": "a\n" }, "base");
      await commit(
        git,
        dir,
        {},
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

Matching modified (option split):
After (matrix):
    Some(x)   _      e1
    None      Some   e2
    order: Some before None

${SIGNED}
`,
      );
    },
    ["HEAD"],
  );
  has(html, "<var>e</var><sub>1</sub>");
  has(html, "<var>τ</var><sub>2</sub>");
  has(html, '<span class="rule-name">(App)</span>');
  // only the premise that changes is shaded
  has(
    html,
    '<div class="premises"><span><var>Γ</var> ⊢ <var>e</var><sub>1</sub>',
  );
  has(html, '<span class="del"><var>Γ</var> ⊢ <var>e</var><sub>2</sub>');
  has(html, '<span class="add"><var>Γ</var> ⊢ <var>e</var><sub>2</sub>');
  assert.equal(html.split('<span class="del">').length - 1, 1);
  assert.equal(html.split('<span class="add">').length - 1, 1);
  has(html, '<div class="conclusion"><div><var>Γ</var> ⊢');
  has(html, "grid-template-columns:repeat(3,max-content)");
  has(html, '<code class="cell"><span class="t">Some</span>(x)</code>');
  has(
    html,
    '<div class="cond"><span class="label">order</span><span class="m">',
  );
});

test("a line of another shape is prose, and a version without its pair is drawn across the block", async () => {
  const html = await render(
    async (git, dir) => {
      await commit(git, dir, { "a.txt": "a\n" }, "base");
      await commit(
        git,
        dir,
        {},
        `Change things

Typing rule modified (x):
After (js):
    let x = 1;

JS modified:
After (js):
    let y = 2;

${SIGNED}
`,
      );
    },
    ["HEAD"],
  );
  has(
    html,
    '<section class="block"><p>Typing rule modified (x):</p><h5 class="form">',
  );
  has(
    html,
    '<section class="block k-js"><h4 class="change"><span class="act modified">modified</span><span class="kind">JS</span></h4>',
  );
  assert.equal(
    html.split(
      '<figure class="one"><figcaption class="vlabel">After</figcaption>',
    ).length - 1,
    2,
  );
  lacks(html, '<div class="rows');
});

/**
 * Renders the record of one commit with the message.
 * @param {string} message
 */
const renderOne = message =>
  render(
    async (git, dir) => {
      await commit(git, dir, { "a.txt": "a\n" }, "base");
      await commit(git, dir, {}, message);
    },
    ["HEAD"],
  );

test("a line with a colon is prose unless it names a section", async () => {
  const html = await renderOne(
    `Keep the node\n\nThe fix is simple: the parser keeps the node.\n\n${SIGNED}\n`,
  );
  has(html, "<p>The fix is simple: the parser keeps the node.</p>");
  lacks(html, '<h4 class="change">');
});

test("a section's name wrapped onto a line of a paragraph is prose", async () => {
  const html = await renderOne(
    `Require a description\n\nIt requires prose or a\nWhy: section, and an example.\nWhy: alone after a blank line is a section.\n\nWhy: Here it is.\n\n${SIGNED}\n`,
  );
  has(
    html,
    "<p>It requires prose or a Why: section, and an example. Why: alone after a blank line is a section.</p>",
  );
  assert.equal(html.split('<section class="block k-why">').length - 1, 1);
  has(html, "<p>Here it is.</p>");
});

test("a character literal with a Unicode escape is one string token", async () => {
  const html = await renderOne(
    `Match a range\n\nExample:\n    switch c { | '\\u{100}' .. '\\u{1100}' => 1 | _ => 0 }\n\n${SIGNED}\n`,
  );
  has(html, `<span class="s">'\\u{100}'</span>`);
  has(html, `<span class="s">'\\u{1100}'</span>`);
});

test("Breaking and Review sections right above the sign-off are drawn", async () => {
  const html = await renderOne(
    `Rename a flag\n\nBreaking: -bs-x is now -x.\nReview: Is -bs-x still read? No.\n${SIGNED}\nCo-Authored-By: B Person <b@example.com>\n`,
  );
  has(
    html,
    '<div class="banner"><strong>Breaking</strong> -bs-x is now -x.</div>',
  );
  has(html, '<article class="card breaking"');
  has(html, '<ul class="review"><li>Is -bs-x still read? No.</li></ul>');
  has(html, "</code> · A Person + B Person · ");
  lacks(html, "Signed-off-by");
});

test("a block ending right above the sign-off keeps the sign-off out", async () => {
  const html = await renderOne(
    `Change x\n\nJS modified (x):\nAfter (js):\n    let x = 1;\n${SIGNED}\n`,
  );
  has(html, '<span class="k">let</span> x = <span class="n">1</span>;');
  lacks(html, "Signed-off-by");
  lacks(html, "A Person &lt;");
});

test("a pair of rewrites shades each side where it changes", async () => {
  const html = await renderOne(
    `Simplify additions\n\nLambda modified (identity):\nBefore (lambda):\n    (+ x 0) ⇝ (+ x 0)\nAfter (lambda):\n    (+ x 0) ⇝ x\n    when: x is an int\n\n${SIGNED}\n`,
  );
  has(html, '<div class="versions">');
  // the left-hand sides are equal and unshaded; the right-hand sides differ
  const lhs =
    '<span class="ln" style="--i:0"><span class="paren">(</span>+ x <span class="n">0</span><span class="paren">)</span></span>';
  assert.equal(html.split(lhs).length - 1, 2);
  has(
    html,
    '<span class="ln del" style="--i:0"><span class="paren">(</span>+ x <span class="n">0</span><span class="paren">)</span></span>',
  );
  has(html, '<span class="ln add" style="--i:0">x</span>');
  has(html, '<div class="cond add"><span class="label">when</span>');
});

test("a chain of rewrites draws every term", async () => {
  const html = await renderOne(
    `Simplify twice\n\nLambda added (chain):\nAfter (lambda):\n    (+ (+ x 0) 0) ⇝ (+ x 0) ⇝ x\n\n${SIGNED}\n`,
  );
  has(html, '<div class="rewrite" style="--n:2">');
  assert.equal(html.split('<span class="to">⇝</span>').length - 1, 2);
  has(html, '<span class="ln" style="--i:0">x</span>');
  has(
    html,
    '<span class="ln" style="--i:0"><span class="paren">(</span>+ x <span class="n">0</span><span class="paren">)</span></span>',
  );
});

test("trailers of any key end the message and are not drawn", async () => {
  const html = await renderOne(
    `Fix a thing\n\nCo-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_x\n${SIGNED}\n`,
  );
  has(html, " · A Person + Claude · ");
  lacks(html, "Opus");
  lacks(html, "Claude-Session");
  lacks(html, "session_x");
  lacks(html, '<section class="block');
});

test("one-line versions are the versions, and tab-indented bodies are blocks", async () => {
  const html = await renderOne(
    `Drop an identity\n\nJS modified (x):\nBefore (js): return x + 0 | 0;\nAfter (js): return x;\n\nJS modified (y):\nBefore (js):\n\treturn y;\nAfter (js):\n\treturn y;\n\n${SIGNED}\n`,
  );
  has(
    html,
    '<span class="ln del" style="--r:2;--c:1;--i:0"><span class="k">return</span> x + <span class="n">0</span> | <span class="n">0</span>;</span>',
  );
  has(
    html,
    '<span class="ln add" style="--r:2;--c:2;--i:0"><span class="k">return</span> x;</span>',
  );
  lacks(html, '<span class="cap">');
  assert.equal(html.split("Before and After are equal").length - 1, 1);
  has(html, '<span class="k">return</span> y;');
});

test("a blank line ends a section whose text is on its heading line", async () => {
  const html = await renderOne(
    `Rename a flag\n\nWhy: the problem.\n\nA separate paragraph.\n\nBreaking: -x is gone.\n\nAnother paragraph.\n\n${SIGNED}\n`,
  );
  has(
    html,
    '<section class="block k-why"><h4 class="change"><span class="kind">Why</span></h4><p>the problem.</p></section>',
  );
  has(html, '<section class="block"><p>A separate paragraph.</p></section>');
  has(html, '<div class="banner"><strong>Breaking</strong> -x is gone.</div>');
  has(html, '<section class="block"><p>Another paragraph.</p></section>');
});

test("a line indented by 2 spaces continues its paragraph", async () => {
  const html = await renderOne(
    `Change things\n\n- first bullet that wraps\n  onto a second line\n\n${SIGNED}\n`,
  );
  has(html, "<p>- first bullet that wraps onto a second line</p>");
  lacks(html, '<pre class="code">');
});

test("a block indented under a paragraph is code", async () => {
  const html = await renderOne(
    `Change output\n\nThe new output for:\n\n    let f = x => x + 1\n    let g = y => y\n\n${SIGNED}\n`,
  );
  has(html, "<p>The new output for:</p>");
  has(
    html,
    '<pre class="code"><span class="ln" style="--i:0"><span class="k">let</span> f = x =&gt; x + <span class="n">1</span></span><span class="ln" style="--i:0"><span class="k">let</span> g = y =&gt; y</span></pre>',
  );
});

test("Example: with text on its line is prose; alone, it heads a program", async () => {
  const html = await renderOne(
    `Fold constants\n\nExample: when the argument is constant, the call folds.\n\nJS modified (f):\nExample:\n    let f = 1\nAfter (js):\n    let f = 1;\n\n${SIGNED}\n`,
  );
  has(
    html,
    '<section class="block"><p>Example: when the argument is constant, the call folds.</p></section>',
  );
  has(
    html,
    '<pre class="code"><span class="ln" style="--i:0"><span class="k">let</span> f = <span class="n">1</span></span></pre>',
  );
});

test("a representation, a matrix and captions are drawn on aligned rows", async () => {
  const html = await renderOne(`Unbox options

Representation modified (option):
Before (representation): boxed
    Some(x) ↦ {_0: x}
    None ↦ undefined
After (representation): unboxed
    Some(x) ↦ x
    None ↦ undefined
    note: x is not undefined

Matching modified (option split):
Before (matrix):
    Some(x)   e1
    None   e2
After (matrix):
    Some(x)   _   e1
    None   e2

JS modified (f):
Before (js): same output
    let f = 1;
After (js): same output
    let f = 1;

${SIGNED}
`);
  has(
    html,
    '<div class="vlabel del">Before <span class="cap">boxed</span></div><div class="vlabel add">After <span class="cap">unboxed</span></div>',
  );
  has(
    html,
    '<span class="ln add" style="--r:2;--c:2;--i:0"><span class="t">Some</span>(x) <span class="to">↦</span> x</span>',
  );
  has(
    html,
    '<span class="ln" style="--r:3;--c:1;--i:0"><span class="t">None</span>    <span class="to">↦</span> <span class="k">undefined</span></span>',
  );
  has(html, '<div class="cond"><span class="label">note</span>');
  has(
    html,
    '<span class="ln add" style="--r:2;--c:2;--i:0"><span class="t">Some</span>(x)   _    e1</span>',
  );
  has(
    html,
    '<span class="ln" style="--r:3;--c:1;--i:0"><span class="t">None</span>      e2</span>',
  );
  has(
    html,
    '<figcaption class="vlabel">Before and After are equal <span class="cap">same output</span></figcaption>',
  );
  assert.equal(html.split('<div class="rows code">').length - 1, 2);
});

test("typed, expanded and lambda forms, and links in prose", async () => {
  let first = "";
  const html = await render(
    async (git, dir) => {
      await commit(git, dir, { "a.txt": "a\n" }, "base");
      await commit(git, dir, {}, `Type raw nodes\n\n${SIGNED}\n`);
      first = git("rev-parse", "--short=9", "HEAD").trim();
      await commit(
        git,
        dir,
        {},
        `Lower raw nodes

Why:
    ${first} types raw nodes; #1234 lowers them.

Typedtree added (raw):
After (typed):
    Texp_raw "x"

AST modified (raw):
Before (expanded):
    Pexp_extension "raw"
After (expanded):
    Pexp_raw "raw"

Lambda added (raw):
After (lambda):
    (let (f/1002 = (#raw_expr "x")) f/1002)

${SIGNED}
`,
      );
    },
    ["HEAD~2..HEAD"],
  );
  has(
    html,
    `<a href="#c-${first}"><code>${first}</code></a> types raw nodes; <a href="https://github.com/rescript-lang/rescript/pull/1234">#1234</a> lowers them.`,
  );
  has(html, '<span class="lang">typed</span>');
  has(
    html,
    '<span class="t">Texp_raw</span> <span class="s">&quot;x&quot;</span>',
  );
  has(html, '<span class="lang">expanded</span>');
  has(html, '<div class="rows code tree">');
  has(html, '<span class="lang">lambda</span>');
  has(html, '<span class="k">let</span>');
  has(html, '<span class="stamp">/1002</span>');
  has(html, '<span class="p">#raw_expr</span>');
});

const DIVIDE = `Exception Analysis
Division.res:1:5-10
divide might throw Division_by_zero (Division.res:1:27) and is not annotated with @throws(Division_by_zero)`;
const DIVIDE_BY = `Exception Analysis
Division.res:2:5-12
divideBy might throw Division_by_zero (Division.res:2:37) and is not annotated with @throws(Division_by_zero)`;

const DEAD = `Warning Dead Value
DeadTest.res:3:1-10
deadValue is never used`;

test("a reanalyze report draws its findings aligned by identity, one analysis tagging the form", async () => {
  const html = await renderOne(`Skip literal divisors

Reanalyze modified (division):
Before (reanalyze):
${indented(`\n${DIVIDE}\n\n${DIVIDE_BY}`, "      ")}
After (reanalyze):
${indented(`\n${DIVIDE_BY}`, "      ")}

${SIGNED}
`);
  has(
    html,
    '<h5 class="form"><span class="lang">reanalyze</span> <span class="tag">Exception Analysis</span></h5>',
  );
  has(html, '<div class="report rows-report">');
  has(
    html,
    '<div class="finding del" style="--r:1;--c:1"><div class="where"><code class="loc">Division.res:1:5-10</code></div><div class="msg">divide might throw Division_by_zero (<span class="ref">Division.res:1:27</span>) and is not annotated with <span class="a">@throws(Division_by_zero)</span></div></div>',
  );
  has(html, '<div class="finding gap" style="--r:1;--c:2"></div>');
  has(
    html,
    '<details class="same" style="--r:2"><summary>1 unchanged finding</summary><div class="report rows-report"><div class="finding" style="--r:1;--c:1"><div class="where"><code class="loc">Division.res:2:5-12</code>',
  );
  has(html, '<div class="finding dup" style="--r:1;--c:2">');
  assert.equal(html.split('<div class="finding del"').length - 1, 1);
  lacks(html, '<div class="finding add"');
  lacks(html, '<div class="where"><span class="tag">');
});

test("an added finding is shaded added, and findings of several analyses keep their tags", async () => {
  const html = await renderOne(`Report dead values

Reanalyze modified (dead code):
Before (reanalyze):
${indented(`${DIVIDE}\n\nAnalysis reported 1 issues`, "    ")}
After (reanalyze):
${indented(`${DEAD}\n\n${DIVIDE}\n\nAnalysis reported 2 issues (Warning Dead Value:1)`, "    ")}

Reanalyze added (termination):
After (reanalyze):
    Error Termination
    TestCyberTruck.res:29:28-39
    Possible infinite loop when calling alwaysLoop
    CallStack:
      1 alwaysLoop (TestCyberTruck.res:29)

${SIGNED}
`);
  has(html, '<h5 class="form"><span class="lang">reanalyze</span></h5>');
  has(
    html,
    '<div class="finding add" style="--r:1;--c:2"><div class="where"><span class="tag">Warning Dead Value</span><code class="loc">DeadTest.res:3:1-10</code></div><div class="msg">deadValue is never used</div></div>',
  );
  has(html, '<div class="finding gap" style="--r:1;--c:1"></div>');
  has(
    html,
    '<summary>1 unchanged finding</summary><div class="report rows-report"><div class="finding" style="--r:1;--c:1"><div class="where"><span class="tag">Exception Analysis</span>',
  );
  has(
    html,
    '<div class="finding note mod" style="--r:3;--c:1"><div class="msg">Analysis reported <span class="del">1</span> issues</div></div>',
  );
  has(
    html,
    '<div class="finding note mod" style="--r:3;--c:2"><div class="msg">Analysis reported <span class="add">2</span> issues <span class="add">(</span>',
  );
  has(
    html,
    '<h5 class="form"><span class="lang">reanalyze</span> <span class="tag">Error Termination</span></h5><figure class="one"><figcaption class="vlabel">After</figcaption><div class="report"><div class="finding"><div class="where"><code class="loc">TestCyberTruck.res:29:28-39</code></div><div class="msg">Possible infinite loop when calling alwaysLoop\nCallStack:\n  1 alwaysLoop (TestCyberTruck.res:29)</div></div></div></figure>',
  );
});

test("a run of unchanged findings is one row, and a changed count is one row", async () => {
  const html = await renderOne(`Skip literal divisors

Reanalyze modified (division):
Before (reanalyze):
${indented(`\n${DIVIDE}\n\n${DIVIDE_BY}\n\n${DIVIDE_BY.replaceAll("divideBy", "divideTo")}\n\nAnalysis reported 3 issues (Exception Analysis:3)`, "      ")}
After (reanalyze):
${indented(`\n${DIVIDE_BY}\n\n${DIVIDE_BY.replaceAll("divideBy", "divideTo")}\n\nAnalysis reported 2 issues (Exception Analysis:2)`, "      ")}

${SIGNED}
`);
  has(
    html,
    '<details class="same" style="--r:2"><summary>2 unchanged findings</summary>',
  );
  has(html, '<div class="finding" style="--r:2;--c:1"><div class="where">');
  has(html, '<div class="finding dup" style="--r:2;--c:2"><div class="where">');
  has(html, "divideTo might throw");
  assert.equal(html.split('<details class="same"').length - 1, 1);
  has(
    html,
    '<div class="finding note mod" style="--r:3;--c:1"><div class="msg">Analysis reported <span class="del">3</span> issues (Exception Analysis:<span class="del">3</span>)</div></div>',
  );
  has(
    html,
    '<div class="finding note mod" style="--r:3;--c:2"><div class="msg">Analysis reported <span class="add">2</span> issues (Exception Analysis:<span class="add">2</span>)</div></div>',
  );
  lacks(html, '<div class="finding note del"');
  lacks(html, '<div class="finding note add"');
  has(
    html,
    ".finding { background:var(--surface); border:1px solid var(--rule); border-radius:6px;",
  );
});

test("an elision in a report is a muted row, and in code a muted line", async () => {
  const html = await renderOne(`Skip literal divisors

Reanalyze modified (division):
Before (reanalyze):
${indented(`${DIVIDE}\n\n… 4 more like this\n\n${DIVIDE_BY}`, "    ")}
After (reanalyze):
${indented(`${DIVIDE_BY}`, "    ")}

JS modified (division):
Before (js):
    let a = 1;
    …
After (js):
    let a = 2;
    … 3 more lines

${SIGNED}
`);
  has(
    html,
    '<div class="finding elide" style="--r:2;--c:1"><div class="msg">… 4 more like this</div></div>',
  );
  has(html, '<div class="finding gap" style="--r:2;--c:2"></div>');
  lacks(html, "finding elide del");
  has(html, '<span class="ln elide" style="--r:3;--c:1;--i:0">…</span>');
  has(html, '<span class="ln elide" style="--r:3;--c:2;--i:0">… ');
  has(html, '<span class="ln del" style="--r:2;--c:1;--i:0">');
});

test("--markdown draws every commit into the comment, with the page's link", async () => {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), "commit-records-"));
  const git = (/** @type {string[]} */ ...a) =>
    execFileSync("git", a, { cwd: dir, encoding: "utf8" }).trim();
  git("init", "-q", "-b", "main");
  git("config", "user.name", "A Person");
  git("config", "user.email", "a@example.com");
  await commit(git, dir, { "a.txt": "a\n" }, "base");
  await commit(git, dir, {}, `First change\n\n${SIGNED}`);
  await commit(git, dir, {}, `Second change\n\n${SIGNED}`);
  const md = path.join(dir, "comment.md");
  const r = spawnSync(
    "node",
    [
      script,
      ...["--out", path.join(dir, "page.html"), "--markdown", md],
      ...["--url", "https://cdn.example/pr-1/index.html", "HEAD~2..HEAD"],
    ],
    { cwd: dir, encoding: "utf8" },
  );
  assert.equal(r.status, 0, r.stderr);
  const comment = await fs.readFile(md, "utf8");
  const tip = git("rev-parse", "--short=9", "HEAD");
  assert.match(
    comment,
    new RegExp(
      `^### \\[Semantic Diff\\]\\(https://cdn\\.example/pr-1/index\\.html\\)\\n\\n2 commits · ${tip} · [A-Z][a-z]{2} \\d\\d, \\d{4}, \\d\\d:\\d\\d UTC · \\[Full page\\]\\(https://cdn\\.example/pr-1/index\\.html\\)\\n\\n<details`,
    ),
  );
  has(comment, "First change");
  has(comment, "Second change");
});

test("--previous marks the commits the last push added or edited", async () => {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), "commit-records-"));
  const git = (/** @type {string[]} */ ...a) =>
    execFileSync("git", a, { cwd: dir, encoding: "utf8" }).trim();
  git("init", "-q", "-b", "main");
  git("config", "user.name", "A Person");
  git("config", "user.email", "a@example.com");
  await commit(git, dir, { "a.txt": "a\n" }, "base");
  const base = git("rev-parse", "HEAD");
  await commit(git, dir, {}, `Kept\n\n${SIGNED}`);
  await commit(git, dir, {}, `Edited\n\nFirst words.\n\n${SIGNED}`);
  await commit(git, dir, {}, `Dropped\n\n${SIGNED}`);
  const previous = git("rev-parse", "HEAD");
  // A force push: the last commit dropped, the one below reworded, one added.
  git("reset", "-q", "--hard", "HEAD~2");
  await commit(git, dir, {}, `Edited\n\nOther words.\n\n${SIGNED}`);
  await commit(git, dir, {}, `Added\n\n${SIGNED}`);
  const md = path.join(dir, "comment.md");
  const r = spawnSync(
    "node",
    [
      script,
      ...["--out", path.join(dir, "page.html"), "--markdown", md],
      ...["--previous", previous, `${base}..HEAD`],
    ],
    { cwd: dir, encoding: "utf8" },
  );
  assert.equal(r.status, 0, r.stderr);
  const comment = await fs.readFile(md, "utf8");
  has(comment, "<summary><b>Kept</b></summary>");
  const marked = (/** @type {string} */ subject) =>
    comment.split("<details").find(d => d.includes(`<b>${subject}</b>`)) ?? "";
  lacks(marked("Kept"), "✨");
  lacks(marked("Kept"), "✏️");
  has(marked("Edited"), "<summary>✏️ <b>Edited</b></summary>");
  has(marked("Edited"), " · edited</sub>");
  has(marked("Added"), "<summary>✨ <b>Added</b></summary>");
  has(marked("Added"), " · new</sub>");
});
