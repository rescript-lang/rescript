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

Example (rescript):
    let f = (x: int) => x + 0

Before (js):
    return x + 0 | 0;

After (js):
    return x;

Source: bsc before and after this commit.

Area: codegen tests
Output: changed integer + 0; one snapshot
Formats: none
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
  has(html, '<span class="ln del">');
  has(html, '<span class="ln add">');
  has(html, '<p class="source"><b>Source</b>');
  has(html, "Changelog fragment changes/identity.polish.md");
  has(
    html,
    '<div class="entry"><span class="pill cat-polish">💅 Polish</span><span>Compile <code>x + 0</code>',
  );
  has(html, "1 committed generated JS file changes; 1 new");
  has(html, "Formats: none");
  has(html, "<code>tests/tests/src/</code>");
  has(html, "<li>bundle ");
  has(html, "changelog 1");
  has(html, '<td class="num">0</td></tr>');
  has(html, "<li>Compile <code>x + 0</code>");
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
  has(html, "Output: unchanged, but 1 committed generated JS file changes");
  has(html, "No Tests trailer");
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
      git("commit", "-q", "-m", "Draft\n\nArea: compiler");
      await write(dir, {
        "changes/crash.fix.md": `- Fix a crash\n  in the printer. ${link}/8\n- Fix another. ${link}/8\n`,
        "CHANGELOG.md": "# Changelog\n\n- Edited.\n",
      });
      git("rm", "-q", "changes/gone.feature.md");
      git("add", ".");
      git("commit", "-q", "-m", "Link the PR\n\nArea: compiler");
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
  has(html, '<td><span class="pill cat-fix">🐛 Bug fix</span></td>');
  has(
    html,
    '<div><span class="pill cat-fix">🐛 Bug fix</span><ul><li>Fix a crash in the printer. <a href="https://github.com/rescript-lang/rescript/pull/8">#8</a></li><li>Fix another.',
  );
  assert.ok(!html.includes("<li>Add a thing."), "deleted fragment in draft");
});
