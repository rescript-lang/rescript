import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

const script = fileURLToPath(new URL("./commitRecords.js", import.meta.url));

const record = `Compile x + 0 to x

Integer addition of the literal 0 compiles to the operand.

Why:
    \`x + 0\` on an int compiles to \`x + 0 | 0\`, which is \`x\`.

AST modified (Pexp_apply of +):
  Before:
    Pexp_apply
      funct: Pexp_ident "+"
      args:
        Pexp_ident "x"
        Pexp_constant 0
  After:
    Pexp_ident "x"

Example (rescript):
    let f = (x: int) => x + 0

Before (js):
    return x + 0 | 0;

After (js):
    return x;

Source: bsc before and after this commit.

Review: Does it hold for floats? No: \`x +. 0.\` keeps the addition.

Area: codegen tests
Output: changed integer + 0; one snapshot
Tests: +tests/tests/src/a_test.mjs ~tests/tests/src/b_test.mjs
Measured: build 10 s -> 8 s (make, 1 run); bundle 5 KB -> 4 KB (esbuild)
Signed-off-by: A Person <a@example.com>
`;

/**
 * Asserts that the page contains the text, without printing the page.
 * @param {string} html
 * @param {string} text
 */
const has = (html, text) =>
  assert.ok(html.includes(text), `page lacks ${JSON.stringify(text)}`);

/**
 * Renders the records of a temporary repository's commits.
 * @param {(git: (...args: string[]) => string, dir: string) => Promise<void>} setup
 * @param {string[]} revs
 */
async function render(setup, revs) {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), "commit-records-"));
  const git = (/** @type {string[]} */ ...args) =>
    execFileSync("git", args, { cwd: dir, encoding: "utf8" });
  git("init", "-q", "-b", "main");
  git("config", "user.name", "A Person");
  git("config", "user.email", "a@example.com");
  await setup(git, dir);
  const out = path.join(dir, "page.html");
  execFileSync("node", [script, "--out", out, ...revs], { cwd: dir });
  return fs.readFile(out, "utf8");
}

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

test("renders a record and checks it against the diff", async () => {
  const html = await render(
    async (git, dir) => {
      await write(dir, { "tests/tests/src/b_test.mjs": "x + 0 | 0\n" });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, {
        "changes/identity.polish.md":
          "- Compile `x + 0` to `x`. https://github.com/rescript-lang/rescript/pull/1234\n",
        "tests/tests/src/a_test.mjs": "x\n",
        "tests/tests/src/b_test.mjs": "x\n",
      });
      git("add", ".");
      git("commit", "-q", "-m", record);
    },
    ["main~1..main"],
  );
  has(html, "<h3>Compile x + 0 to x</h3>");
  has(html, '<h1 class="brand">Semantic <span>Diff</span></h1>');
  has(html, "<title>Semantic Diff</title>");
  // the JavaScript Before/After pair, side by side in one row, changed
  // lines marked
  has(
    html,
    '<div class="vlabel del">Before <span class="cap"><span class="lang">js</span>',
  );
  has(
    html,
    '<span class="ln del" style="--r:2;--c:1;--i:0"><span class="k">return</span> x + <span class="n">0</span>',
  );
  has(
    html,
    '<span class="ln add" style="--r:2;--c:2;--i:0"><span class="k">return</span> x;</span>',
  );
  has(html, '<p class="source"><b>Source</b>');
  // the AST change block, drawn as two trees in aligned rows
  has(
    html,
    '<h4 class="change"><span class="act modified">modified</span><span class="kind">AST</span> <span class="target">Pexp_apply of +</span></h4>',
  );
  has(html, '<div class="rows code tree">');
  has(
    html,
    '<span class="ln del" style="--r:3;--c:1;--i:3"><span class="branch">├─\u00a0</span><span class="label">funct:</span> <span class="t">Pexp_ident</span>',
  );
  has(
    html,
    // the operand moves to the root, so it is a changed line; After's
    // shorter side is padded with gaps
    '<span class="ln add" style="--r:2;--c:2;--i:0"><span class="branch"></span><span class="t">Pexp_ident</span> <span class="s">&quot;x&quot;</span></span><span class="ln gap" style="--r:3;--c:2;--i:0">',
  );
  has(html, '<section class="block k-why">');
  has(html, '<ul class="review"><li>Does it hold for floats?');
  has(html, "Changelog fragment changes/identity.polish.md");
  has(
    html,
    '<div class="entry"><span class="pill cat-polish">💅 Polish</span><span>Compile <code>x + <span class="n">0</span></code>',
  );
  has(html, "1 committed generated JS file modified, 1 added");
  has(html, "The tests listed are in the diff");
  has(html, "Every test file in the diff is listed");
  has(html, "<code>tests/tests/src/</code>");
  has(html, '<span class="test add" title="tests/tests/src/a_test.mjs">');
  has(html, "<li>bundle ");
  has(html, '<span class="delta"><span class="from">10 s</span>');
  has(html, '<span class="chip">codegen</span>');
  has(html, "no failed checks");
  has(html, "<h4>Changelog draft</h4>");
});

test("flags a record that disagrees with its diff", async () => {
  const html = await render(
    async (git, dir) => {
      await write(dir, { "tests/tests/src/b_test.mjs": "a\n" });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, { "tests/tests/src/b_test.mjs": "b\n" });
      git("add", ".");
      git(
        "commit",
        "-q",
        "-m",
        "Change output\n\nArea: codegen\nOutput: unchanged",
      );
    },
    ["--pr", "42", "--title", "Fix <output> & more", "HEAD"],
  );
  has(
    html,
    '<h1><a href="https://github.com/rescript-lang/rescript/pull/42">#42</a> Fix &lt;output&gt; &amp; more</h1>',
  );
  has(html, "<title>#42 Fix &lt;output&gt; &amp; more · Semantic Diff</title>");
  has(html, "Output: unchanged, but 1 committed generated JS file modified");
  has(html, "No Tests trailer");
  has(html, '<span class="pill cat-none">No changelog</span>');
});

test("accepts Tests: none with a reason, and flags it against test files", async () => {
  const html = await render(
    async (git, dir) => {
      await write(dir, { "README.md": "a\n" });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, { "README.md": "b\n" });
      git("add", ".");
      git(
        "commit",
        "-q",
        "-m",
        "Reword the readme\n\nArea: docs\nOutput: unchanged\nTests: none, because only prose changes",
      );
      await write(dir, { "tests/tests/src/c_test.res": "let x = 1\n" });
      git("add", ".");
      git(
        "commit",
        "-q",
        "-m",
        "Add a test\n\nArea: tests\nOutput: unchanged\nTests: none, because it is a fixture",
      );
    },
    ["HEAD~2..HEAD"],
  );
  has(html, "No test changes, as recorded");
  has(
    html,
    '<span class="state same">none</span><span>because only prose changes</span>',
  );
  has(html, "Tests: none, but the diff changes 1 test file");
  has(html, '<span class="pill cat-none">No changelog</span>');
});

test("checks the tests a Tests trailer lists against the diff", async () => {
  const html = await render(
    async (git, dir) => {
      await write(dir, { "README.md": "a\n" });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, { "tests/tests/src/d_test.res": "let x = 1\n" });
      git("add", ".");
      git(
        "commit",
        "-q",
        "-m",
        "Add a test\n\nArea: tests\nOutput: unchanged\nTests: +tests/tests/src/d_test.res ~tests/tests/src/e_test.res",
      );
    },
    ["HEAD"],
  );
  has(
    html,
    "Tests lists tests/tests/src/e_test.res, which the diff does not change",
  );
});

test("flags test files the diff changes that Tests does not list", async () => {
  const html = await render(
    async (git, dir) => {
      await write(dir, {
        "tests/tests/src/f_test.res": "let x = 1\n",
        "scripts/a.test.js": "a\n",
      });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, {
        "tests/tests/src/f_test.res": "let x = 2\n",
        "tests/tests/src/f_test.mjs": "let x = 2;\n",
        "tests/syntax_tests/data/printer/expected/g.res.txt": "g\n",
        "tests/syntax_tests/data/printer/g.res": "g\n",
        "tests/ounit_tests/h_tests.ml": "let () = ()\n",
      });
      git("rm", "-q", "scripts/a.test.js");
      git("add", ".");
      git(
        "commit",
        "-q",
        "-m",
        "Change tests\n\nArea: tests\nOutput: unchanged\nTests: ~tests/tests/src/f_test.res +tests/syntax_tests/data/printer/g.res",
      );
    },
    ["HEAD"],
  );
  // the generated JavaScript and the expected output are snapshots, which
  // the trailer may leave out
  has(
    html,
    "Tests omits test files the diff changes: -scripts/a.test.js +tests/ounit_tests/h_tests.ml",
  );
  has(html, "The tests listed are in the diff");
});

test("draws an added or removed object across the block", async () => {
  const long = `and raw_source = Raw_string of string loc (* ${"the JavaScript source, ".repeat(6)}*)`;
  const html = await render(
    async (git, dir) => {
      await write(dir, { "README.md": "a\n" });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, { "README.md": "b\n" });
      git("add", ".");
      git(
        "commit",
        "-q",
        "-m",
        `Add raw nodes\n\nIR added (raw_source):\n  After:\n    ${long}\n\nJS removed (braced output):\n  Before (js):\n    var x = 1;\n\nArea: docs\nOutput: unchanged\nTests: none, because only prose changes`,
      );
    },
    ["HEAD"],
  );
  has(
    html,
    '<h4 class="change"><span class="act added">added</span><span class="kind">IR</span> <span class="target">raw_source</span></h4><pre class="code"><span class="ln" style="--i:0"><span class="k">and</span> raw_source',
  );
  has(
    html,
    '<h4 class="change"><span class="act removed">removed</span><span class="kind">JS</span> <span class="target">braced output</span></h4><pre class="code"><span class="ln" style="--i:0"><span class="k">var</span> x',
  );
  assert.ok(!html.includes("none</p>"), "an empty version is drawn");
  assert.ok(!html.includes('class="versions"'), "a one-sided pair is drawn");
  // a long line wraps, its continuation indented
  has(html, "white-space:pre-wrap");
  has(html, "text-indent:calc((var(--i, 0) + 2) * -1ch)");
});

test("shows and checks each of a commit's changelog fragments", async () => {
  const link = "https://github.com/rescript-lang/rescript/pull";
  const html = await render(
    async (git, dir) => {
      await write(dir, { "README.md": "a\n" });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, {
        "changes/printer-crash.fix.md": `- Fix a printer crash. ${link}/9\n`,
        "changes/printer-option.feature.md": `- Add a printer option. ${link}/XXXX\n`,
      });
      git("add", ".");
      git(
        "commit",
        "-q",
        "-m",
        "Fix and extend the printer\n\nArea: printer\nOutput: unchanged\nTests: none, because the fragments are the change",
      );
    },
    ["HEAD~1..HEAD"],
  );
  has(html, "Changelog fragment changes/printer-crash.fix.md");
  has(
    html,
    "changes/printer-option.feature.md: replace the placeholder https://github.com/rescript-lang/rescript/pull/XXXX with the PR link",
  );
  has(
    html,
    '<div class="entry"><span class="pill cat-fix">🐛 Bug fix</span><span>Fix a printer crash. <a href="https://github.com/rescript-lang/rescript/pull/9">#9</a></span></div>',
  );
  has(
    html,
    '<div class="entry"><span class="pill cat-feature">🚀 New feature</span><span>Add a printer option.',
  );
  has(
    html,
    'changelog: <span class="pill cat-fix">🐛 Bug fix</span> <span class="pill cat-feature">🚀 New feature</span>',
  );
  has(html, "1 failed check");
});

test("takes the changelog from the fragments a range leaves", async () => {
  const link = "https://github.com/rescript-lang/rescript/pull";
  const html = await render(
    async (git, dir) => {
      await write(dir, { "CHANGELOG.md": "# Changelog\n" });
      git("add", ".");
      git("commit", "-q", "-m", "base");
      await write(dir, {
        "changes/crash.fix.md": `- Fix a crash. ${link}/XXXX\n`,
        "changes/gone.feature.md": `- Add a thing. ${link}/7\n`,
      });
      git("add", ".");
      git("commit", "-q", "-m", "Draft\n\nArea: compiler\nOutput: unchanged");
      await write(dir, {
        "changes/crash.fix.md": `- Fix a crash\n  in the printer. ${link}/8\n- Fix another. ${link}/8\n`,
        "CHANGELOG.md": "# Changelog\n\n- Edited.\n",
      });
      git("rm", "-q", "changes/gone.feature.md");
      git("add", ".");
      git(
        "commit",
        "-q",
        "-m",
        "Link the PR\n\nArea: compiler\nOutput: unchanged",
      );
    },
    ["HEAD~2..HEAD"],
  );
  has(
    html,
    "changes/crash.fix.md: replace the placeholder https://github.com/rescript-lang/rescript/pull/XXXX with the PR link",
  );
  has(html, "Changelog fragment changes/gone.feature.md");
  has(
    html,
    "Edits CHANGELOG.md; a changes/ fragment carries the entry outside release PRs",
  );
  has(html, 'changelog: <span class="pill cat-fix">🐛 Bug fix</span></li>');
  has(
    html,
    '<div><span class="pill cat-fix">🐛 Bug fix</span><ul><li>Fix a crash in the printer. <a href="https://github.com/rescript-lang/rescript/pull/8">#8</a></li><li>Fix another.',
  );
  assert.ok(!html.includes("<li>Add a thing."), "deleted fragment in draft");
});

test("the commit-msg hook requires the record's trailers", async () => {
  const hook = fileURLToPath(new URL("./commitRecordHook.sh", import.meta.url));
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), "commit-record-hook-"));
  execFileSync("git", ["init", "-q"], { cwd: dir });
  await write(dir, { "compiler/a.ml": "let x = 1\n" });
  execFileSync("git", ["add", "."], { cwd: dir });
  const run = async (/** @type {string} */ message) => {
    await write(dir, { msg: message });
    try {
      execFileSync("bash", [hook, "msg"], { cwd: dir, stdio: "pipe" });
      return "";
    } catch (e) {
      return String(/** @type {{ stderr: Buffer }} */ (e).stderr);
    }
  };
  assert.equal(
    await run(
      "Add a\n\nWhy:\n    a is missing\n\nCode added (a):\n    let x = 1\n\nArea: compiler\nOutput: unchanged\nTests: none, because a is unused\nSigned-off-by: A Person <a@example.com>\n",
    ),
    "",
  );
  const refused = await run(
    "Add a\n\nArea: compiler\nOutput: same\nSigned-off-by: A Person <a@example.com>\nCo-Authored-By: B <b@example.com>\n",
  );
  for (const problem of [
    "no 'Tests:' trailer",
    "the last trailer is 'Co-Authored-By'",
    "Output 'same' starts with neither",
    "no 'Why:' section",
    "no change block",
  ])
    assert.ok(refused.includes(problem), `hook output lacks ${problem}`);
});
